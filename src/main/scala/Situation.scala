package chess

import cats.data.Validated
import cats.implicits.*
import bitboard.Bitboard
import bitboard.Bitboard.*

import chess.format.Uci

case class Situation(board: Board, color: Color):

  lazy val actors = board actorsOf color

  lazy val moves: Map[Pos, List[Move]] =
    this.generateMoves.groupBy(_.orig)

  lazy val playerCanCapture: Boolean = moves.exists(_._2.exists(_.captures))

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _.map(_.dest) }.to(Map)

  def drops: Option[List[Pos]] =
    board.variant match
      case v: variant.Crazyhouse.type => v possibleDrops this
      case _                          => None

  lazy val kingPos: Option[Pos] = board kingPosOf color

  lazy val check: Boolean = board check color

  def checkSquare = if (check) kingPos else None

  inline def history = board.history

  inline def checkMate: Boolean = board.variant checkmate this

  inline def staleMate: Boolean = board.variant staleMate this

  inline def autoDraw: Boolean = board.autoDraw || board.variant.specialDraw(this)

  inline def opponentHasInsufficientMaterial: Boolean = board.variant.opponentHasInsufficientMaterial(this)

  lazy val threefoldRepetition: Boolean = board.history.threefoldRepetition

  inline def variantEnd = board.variant specialEnd this

  inline def end: Boolean = checkMate || staleMate || autoDraw || variantEnd

  inline def winner: Option[Color] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end && !copy(color = !color).check

  lazy val status: Option[Status] =
    if (checkMate) Status.Mate.some
    else if (variantEnd) Status.VariantEnd.some
    else if (staleMate) Status.Stalemate.some
    else if (autoDraw) Status.Draw.some
    else none

  def move(from: Pos, to: Pos, promotion: Option[PromotableRole]): Validated[String, Move] =
    board.variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest, uci.promotion)

  def drop(role: Role, pos: Pos): Validated[String, Drop] =
    board.variant.drop(this, role, pos)

  def withHistory(history: History) =
    copy(
      board = board withHistory history
    )

  def withVariant(variant: chess.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  export board.history.canCastle

  def enPassantSquare: Option[Pos] =
    // Before potentially expensive move generation, first ensure some basic
    // conditions are met.
    // todo we can do better with bitboard
    history.lastMove match
      case Some(move: Uci.Move) =>
        if (
          move.dest.yDist(move.orig) == 2 &&
          board(move.dest).exists(_.is(Pawn)) &&
          List(
            move.dest.file.offset(-1),
            move.dest.file.offset(1)
          ).flatten.flatMap(board(_, color.passablePawnRank)).exists(_ == color.pawn)
        )
          moves.values.flatten.find(_.enpassant).map(_.dest)
        else None
      case _ => None

  def unary_! = copy(color = !color)

  // =======================================bitboard========================

  val ourKing                    = board.board.king(color)
  val us: Bitboard               = board.board.byColor(color)
  def them: Bitboard             = board.board.byColor(!color)
  def checkers: Option[Bitboard] = ourKing.map(board.board.attacksTo(_, !color))
  def sliderBlockers: Bitboard   = board.board.sliderBlockers(color)
  def isWhiteTurn: Boolean       = color.white
  def isOccupied: Pos => Boolean = board.board.isOccupied

  def isSafe(king: Pos, move: Move, blockers: Bitboard): Boolean =
    if move.enpassant then
      val newOccupied =
        (board.occupied ^ move.orig.bitboard ^ move.dest.combine(move.orig).bitboard) | move.dest.bitboard
      (king.rookAttacks(newOccupied) & them & (board.rooks ^ board.queens)) == Bitboard.empty &&
      (king.bishopAttacks(newOccupied) & them & (board.bishops ^ board.queens)) == Bitboard.empty
    else if move.capture.isDefined then true
    else !(us & blockers).contains(move.orig.value) || Bitboard.aligned(move.orig, move.dest, king)

object Situation:

  def apply(variant: chess.variant.Variant): Situation = Situation(Board init variant, White)

  case class AndFullMoveNumber(situation: Situation, fullMoveNumber: FullMoveNumber):
    def ply = fullMoveNumber.ply(situation.color)

  import scala.collection.mutable.ListBuffer

  extension (f: Situation)

    /** The moves without taking defending the king into account */
    def trustedMoves(withCastle: Boolean): List[Move] =
      val enPassantMoves = f.board.history.epSquare.fold(Nil)(genEnPassant)
      // println(s"passant $enPassantMoves")
      val checkers       = f.checkers.getOrElse(Bitboard.empty)
      val targets        = ~f.us
      val withoutCastles = genNonKing(targets) ++ genSafeKing(targets)
      val moves =
        if (withCastle) withoutCastles ++ genCastling()
        else withoutCastles
      moves ++ enPassantMoves

    def generateMoves: List[Move] =
      val enPassantMoves = f.board.history.epSquare.fold(Nil)(genEnPassant)
      // println(s"passant $enPassantMoves")
      val checkers = f.checkers.getOrElse(Bitboard.empty)
      // println(checkers)
      val movesWithoutEnpassant = if checkers == Bitboard.empty then
        val targets        = ~f.us
        val withoutCastles = genNonKing(targets) ++ genSafeKing(targets)
        if f.board.variant.allowsCastling then withoutCastles ++ genCastling()
        else withoutCastles
      else genEvasions(checkers)
      val moves = movesWithoutEnpassant ++ enPassantMoves
      // println(moves)

      f.ourKing.fold(moves)(king =>
        val blockers = f.sliderBlockers
        if blockers != Bitboard.empty || !f.board.history.epSquare.isDefined then
          moves.filter(m => f.isSafe(king, m, blockers))
        else moves
      )

    private def genEnPassant(ep: Pos): List[Move] =
      val pawns = f.us & f.board.board.pawns & ep.pawnAttacks(!f.color)
      // println(s"pawns $pawns")
      val ff: Bitboard => Option[(Pos, Bitboard)] = bb => bb.lsb.map((_, bb & (bb - 1L)))
      List.unfold(pawns)(ff).map(enpassant(_, ep))

    private def genNonKing(mask: Bitboard): List[Move] =
      // println(s"mask $mask")
      genPawn(mask) ++ genKnight(mask) ++ genBishop(mask) ++ genRook(mask) ++ genQueen(mask)

    /** Generate all pawn moves except en passant
     *  This includes
      *   - captures
      *   - single square moves
      *   - double square moves
      * @mask:
      *   bitboard contains empty square or enemy pieces
      *
      *   TODO @mask includes enemy King now, which should not be because
      *   enemy King cannot be captured by law
      */
    private def genPawn(mask: Bitboard): List[Move] =
      val moves = ListBuffer[Move]()

      // println(s"turns ${f.color}")
      // our pawns or captures
      val capturers = f.us & f.board.board.pawns
      // println(s"capturers $capturers")

      val s1: List[List[Move]] = for
        from <- capturers.occupiedSquares
        targets = from.pawnAttacks(f.color) & f.them & mask
        to <- targets.occupiedSquares
      yield genPawnMoves(from, to, true)
      // println(s"s1 $s1")

      // normal pawn moves
      val singleMoves = ~f.board.occupied & {
        if f.isWhiteTurn then (f.board.white & f.board.pawns) << 8
        else (f.board.black & f.board.pawns) >>> 8
      }

      // println(s"singleMoves $singleMoves")
      val doubleMoves =
        ~f.board.occupied &
          (if f.isWhiteTurn then singleMoves << 8 else singleMoves >>> 8) &
          Bitboard.rank(f.color.fourthRank)
      // println(s"doubleMoves $doubleMoves")

      val s2: List[List[Move]] = for
        to <- (singleMoves & mask).occupiedSquares
        from = Pos.at(to.value + (if f.isWhiteTurn then -8 else 8)).get
      yield genPawnMoves(from, to, false)
      // println(s"s2 $s2")

      val s3: List[Move] = for
        to <- (doubleMoves & mask).occupiedSquares
        from = Pos.at(to.value + (if f.isWhiteTurn then -16 else 16)).get
      yield normalMove(from, to, Pawn, false)

      // println(s"s3 $s3")
      s1.flatten ++ s2.flatten ++ s3

    private def genKnight(mask: Bitboard): List[Move] =
      val knights = f.us & f.board.knights
      for
        from <- knights.occupiedSquares
        targets = Bitboard.knightAttacks(from) & mask
        to <- targets.occupiedSquares
      yield normalMove(from, to, Knight, f.isOccupied(to))

    private def genBishop(mask: Bitboard): List[Move] =
      val bishops = f.us & f.board.bishops
      for
        from <- bishops.occupiedSquares
        targets = from.bishopAttacks(f.board.occupied) & mask
        to <- targets.occupiedSquares
      yield normalMove(from, to, Bishop, f.isOccupied(to))

    private def genRook(mask: Bitboard): List[Move] =
      val rooks = f.us & f.board.rooks
      for
        from <- rooks.occupiedSquares
        targets = from.rookAttacks(f.board.occupied) & mask
        to <- targets.occupiedSquares
      yield normalMove(from, to, Rook, f.isOccupied(to))

    private def genQueen(mask: Bitboard): List[Move] =
      val queens = f.us & f.board.queens
      for
        from <- queens.occupiedSquares
        targets = from.queenAttacks(f.board.occupied) & mask
        to <- targets.occupiedSquares
      yield normalMove(from, to, Queen, f.isOccupied(to))

    private def genEvasions(checkers: Bitboard): List[Move] =
      f.ourKing.fold(Nil)(king =>
        // Checks by these sliding pieces can maybe be blocked.
        val sliders = checkers & (f.board.sliders)
        // println(s"sliders: $sliders")
        val attacked = sliders.occupiedSquares.foldRight(0L)((s, a) =>
          a | (Bitboard.RAYS(king.value)(s.value) ^ (1L << s.value))
        )
        val safeKings = genSafeKing(~f.us & ~attacked)
        // println(s"safeKings $safeKings")
        val blockers =
          if !checkers.moreThanOne then
            checkers.lsb.map(c => genNonKing(Bitboard.between(king, c) | checkers)).getOrElse(Nil)
          else Nil
        // println(s"blockers $blockers")
        safeKings ++ blockers
      )

    // this can still generate unsafe king moves
    private def genSafeKing(mask: Bitboard): List[Move] =
      f.ourKing.fold(Nil)(king =>
        val targets = king.kingAttacks & mask
        for
          to <- targets.occupiedSquares
          if f.board.board.attacksTo(to, !f.color).isEmpty
        yield normalMove(king, to, King, f.isOccupied(to))
      )

    // todo works with standard only
    private def genCastling(): List[Move] =
      f.ourKing.fold(Nil) { king =>
        val firstRank = f.color.backRank
        val rooks     = f.board.history.castles & Bitboard.rank(firstRank) & f.board.rooks
        for
          rook <- rooks.occupiedSquares
          path = Bitboard.between(king, rook)
          if (path & f.board.occupied).isEmpty
          toKingRank = if rook < king then Pos.C1 else Pos.G1
          toRookRank = if rook < king then Pos.D1 else Pos.F1
          kingTo     = toKingRank.combine(king)
          rookTo     = toRookRank.combine(rook)
          kingPath   = Bitboard.between(king, kingTo) | (1L << kingTo.value) | (1L << king.value)
          safe = kingPath.occupiedSquares
            .map(f.board.board.attacksTo(_, !f.color, f.board.occupied ^ (1L << king.value)).isEmpty)
            .forall(identity)
          if safe
          moves <- castle(king, kingTo, rook, rookTo)
        yield moves
      }

    private def genPawnMoves(from: Pos, to: Pos, capture: Boolean): List[Move] =
      if from.rank == f.color.seventhRank then
        List(Queen, Knight, Rook, Bishop).map(promotion(from, to, _, capture))
      else List(normalMove(from, to, Pawn, capture))

    private def enpassant(orig: Pos, dest: Pos) =
      val capture = Option(dest.combine(orig))
      val after   = f.board.taking(orig, dest, capture).get // todo we know that we have value
      Move(
        piece = f.color.pawn,
        orig = orig,
        dest = dest,
        situationBefore = f,
        after = after,
        capture = capture,
        castle = None,
        promotion = None,
        enpassant = true
      )

    private def normalMove(orig: Pos, dest: Pos, role: Role, capture: Boolean) =
      val taken = if capture then Option(dest) else None
      val after =
        if (capture) then f.board.taking(orig, dest, taken).get // todo no get pls
        else f.board.move(orig, dest).get
      Move(
        piece = Piece(f.color, role),
        orig = orig,
        dest = dest,
        situationBefore = f,
        after = after,
        capture = taken,
        castle = None,
        promotion = None,
        enpassant = false
      )

    private def promotion(orig: Pos, dest: Pos, promotion: PromotableRole, capture: Boolean) =
      val taken = if capture then Option(dest) else None
      val after =
        if (capture) then f.board.taking(orig, dest, taken).get // todo no get pls
        else f.board.move(orig, dest).get
      // todo yo lol
      val yo = after.putOrReplace(Piece(f.color, promotion), dest)
      Move(
        piece = f.color.pawn,
        orig = orig,
        dest = dest,
        situationBefore = f,
        after = yo,
        capture = taken,
        castle = None,
        promotion = Some(promotion),
        enpassant = false
      )

    private def castle(king: Pos, kingTo: Pos, rook: Pos, rookTo: Pos): List[Move] =
      // println(s"castle $king $kingTo $rook $rookTo")
      val after = for
        b1    <- f.board.take(king)
        b2    <- b1.take(rook)
        b3    <- b2.place(f.color.king, kingTo)
        after <- b3.place(f.color.rook, rookTo)
      yield after
      // for BC, we add a move where the king goes to the rook position
      for
        a            <- after.toList
        inputKingPos <- List(kingTo, rook).distinct
      yield Move(
        piece = f.color.king,
        orig = king,
        dest = inputKingPos,
        situationBefore = f,
        after = a,
        capture = None,
        castle = Option((king, kingTo), (rook, rookTo)),
        promotion = None,
        enpassant = false
      )
