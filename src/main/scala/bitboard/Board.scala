package chess
package bitboard

import cats.syntax.all.*

import Bitboard.*

// Chess board representation
case class Board(
    occupied: Bitboard,
    byColor: ByColor[Bitboard],
    byRole: ByRole[Bitboard]
):
  val white   = byColor.white
  val black   = byColor.black
  val pawns   = byRole.pawn
  val knights = byRole.knight
  val bishops = byRole.bishop
  val rooks   = byRole.rook
  val queens  = byRole.queen
  val kings   = byRole.king

  def sliders                     = bishops ^ rooks ^ queens
  def isOccupied(s: Pos): Boolean = occupied.contains(s)

  lazy val nbPieces = occupied.count

  def byPiece(piece: Piece): Bitboard =
    byColor(piece.color) & byRole(piece.role)

  def roleAt(s: Pos): Option[Role] =
    if pawns.contains(s) then Some(Pawn)
    else if knights.contains(s) then Some(Knight)
    else if bishops.contains(s) then Some(Bishop)
    else if rooks.contains(s) then Some(Rook)
    else if queens.contains(s) then Some(Queen)
    else if kings.contains(s) then Some(King)
    else None

  def colorAt(s: Pos): Option[Color] =
    if white.contains(s) then Some(Color.White)
    else if black.contains(s) then Some(Color.Black)
    else None

  def pieceAt(s: Pos): Option[Piece] =
    for
      color <- colorAt(s)
      role  <- roleAt(s)
    yield Piece(color, role)

  def whiteAt(s: Pos): Boolean =
    white.contains(s)

  def blackAt(s: Pos): Boolean =
    black.contains(s)

  def kings(color: Color): List[Pos] =
    (kings & byColor(color)).squares

  def kingOf(c: Color): Bitboard       = kings & byColor(c)
  def kingPosOf(c: Color): Option[Pos] = kingOf(c).singleSquare

  def attackers(s: Pos, attacker: Color): Bitboard =
    attackers(s, attacker, occupied)

  def attacks(s: Pos, attacker: Color): Boolean =
    attackers(s, attacker).nonEmpty

  def attackers(s: Pos, attacker: Color, occupied: Bitboard): Bitboard =
    byColor(attacker) & (
      s.rookAttacks(occupied) & (rooks ^ queens) |
        s.bishopAttacks(occupied) & (bishops ^ queens) |
        s.knightAttacks & knights |
        s.kingAttacks & kings |
        s.pawnAttacks(!attacker) & pawns
    )

  // is a king of this color in check
  def isCheck(color: Color): Check =
    Check(kings(color).exists(attacks(_, !color)))

  /** Find all blockers between the king and attacking sliders First we find all snipers (all potential sliders which
    * can attack the king) Then we loop over those snipers if there is only one blockers between the king and the sniper
    * we add them into the blockers list
    *
    * This is being used when checking a move is safe for the king or not
    */
  def sliderBlockers(ourKing: Pos, us: Color): Bitboard =
    val snipers = byColor(!us) & (
      ourKing.rookAttacks(Bitboard.empty) & (rooks ^ queens) |
        ourKing.bishopAttacks(Bitboard.empty) & (bishops ^ queens)
    )
    snipers.squares.foldLeft(Bitboard.empty) { case (blockers, sniper) =>
      val between = Bitboard.between(ourKing, sniper) & occupied
      if between.moreThanOne then blockers
      else blockers | between
    }

  def discard(s: Pos): Board =
    discard(s.bb)

  def discard(mask: Bitboard): Board =
    val notMask = ~mask
    Board(
      occupied & notMask,
      byColor.map(_ & notMask),
      byRole.map(_ & notMask)
    )

  def roles: Role => Bitboard                  = byRole.apply
  def byRoleOf(color: Color): ByRole[Bitboard] = byRole.map(_ & byColor(color))

  // put a piece to an empty square
  def put(piece: Piece, at: Pos): Option[Board] =
    !isOccupied(at) option putOrReplace(piece, at)

  // put a piece to an occupied square
  def replace(piece: Piece, at: Pos): Option[Board] =
    isOccupied(at) option putOrReplace(piece, at)

  // put a piece into the board
  def putOrReplace(s: Pos, role: Role, color: Color): Board =
    val b = discard(s)
    val m = s.bb
    Board(
      b.occupied | m,
      b.byColor.update(color, _ | m),
      b.byRole.update(role, _ | m)
    )

  // put a piece into the board
  // remove the existing piece at that square if needed
  def putOrReplace(p: Piece, s: Pos): Board =
    putOrReplace(s, p.role, p.color)

  def take(at: Pos): Option[Board] =
    isOccupied(at) option discard(at)

  // move without capture
  def move(orig: Pos, dest: Pos): Option[Board] =
    if isOccupied(dest) then None
    else pieceAt(orig).map(discard(orig).putOrReplace(_, dest))

  def taking(orig: Pos, dest: Pos, taking: Option[Pos] = None): Option[Board] =
    for
      piece <- pieceAt(orig)
      takenPos = taking getOrElse dest
      if isOccupied(takenPos)
    yield discard(orig).discard(takenPos).putOrReplace(piece, dest)

  def promote(orig: Pos, dest: Pos, piece: Piece): Option[Board] =
    take(orig).map(_.putOrReplace(piece, dest))

  lazy val occupation: ByColor[Set[Pos]] = ByColor { c =>
    color(c).squares.toSet
  }

  inline def isOccupied(inline p: Piece) =
    piece(p).nonEmpty

  // TODO remove unsafe get
  lazy val pieceMap: Map[Pos, Piece] =
    occupied.squares.view.map(s => (s, pieceAt(s).get)).toMap

  def piecesOf(c: Color): Map[Pos, Piece] =
    pieceMap.filter((_, p) => p.color == c)

  def pieces: List[Piece] = pieces(occupied)

  def pieces(occupied: Bitboard): List[Piece] =
    occupied.squares.flatMap(pieceAt)

  def color(c: Color): Bitboard = c.fold(white, black)

  def piece(p: Piece): Bitboard = color(p.color) & byRole(p.role)

object Board:

  val empty: Board = Board(
    Bitboard.empty,
    ByColor(Bitboard.empty, Bitboard.empty),
    ByRole(Bitboard.empty, Bitboard.empty, Bitboard.empty, Bitboard.empty, Bitboard.empty, Bitboard.empty)
  )

  def fromMap(pieces: PieceMap): Board =
    var pawns    = Bitboard.empty
    var knights  = Bitboard.empty
    var bishops  = Bitboard.empty
    var rooks    = Bitboard.empty
    var queens   = Bitboard.empty
    var kings    = Bitboard.empty
    var white    = Bitboard.empty
    var black    = Bitboard.empty
    var occupied = Bitboard.empty

    pieces.foreach { (s, p) =>
      val position = s.bb
      occupied |= position
      p.role match
        case Pawn   => pawns |= position
        case Knight => knights |= position
        case Bishop => bishops |= position
        case Rook   => rooks |= position
        case Queen  => queens |= position
        case King   => kings |= position

      p.color match
        case Color.White => white |= position
        case Color.Black => black |= position
    }
    Board(occupied, ByColor(white, black), ByRole(pawns, knights, bishops, rooks, queens, kings))
