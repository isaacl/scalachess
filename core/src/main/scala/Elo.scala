package chess

import cats.syntax.all.*

opaque type Elo = Int

opaque type KFactor = Int
object KFactor extends OpaqueInt[KFactor]:
  val default = KFactor(40)

/*
 * https://handbook.fide.com/chapter/B022022
 * https://ratings.fide.com/calc.phtml
 * */
object Elo extends OpaqueInt[Elo]:

  def computeRatingDiff(player: Player, games: Seq[Game]): Int =
    computeNewRating(player, games) - player.rating

  def computeNewRating(player: Player, games: Seq[Game]): Elo =
    val expectedScore = games.foldMap: game =>
      val prd = playersRatingDiff(player.rating, game.opponentRating)
      getExpectedScore(prd)
    val achievedScore = games.foldMap(_.points.value)
    val ratingDiff =
      Math.round(player.kFactor * (achievedScore - expectedScore))
    player.rating + ratingDiff

  /* 8.3.1
   * For each game played against a rated player, determine the difference in rating between the player and their opponent.
   * A difference in rating of more than 400 points shall be counted for rating purposes as though it were a difference of 400 points.  In any tournament, a player may benefit from only one upgrade under this rule, for the game in which the rating difference is greatest. */
  def playersRatingDiff(a: Elo, b: Elo): Int =
    Math.min(400, Math.max(-400, b - a))

  def getExpectedScore(ratingDiff: Int): Float =
    val absRatingDiff = ratingDiff.abs
    val expectedScore = conversionTableFIDE.getOrElse(absRatingDiff, 0.92f)
    if ratingDiff <= 0 then expectedScore else 1.0f - expectedScore

  def computePerformanceRating(games: Seq[Game]): Option[Elo] =
    val winBonus = 400
    games.nonEmpty.option:
      val ratings = games.map(_.opponentRating).sum
      val points = games.foldMap:
        _.points.match
          case Outcome.Points.Zero => -1
          case Outcome.Points.Half => 0
          case Outcome.Points.One  => 1
      (ratings + points * winBonus) / games.size

  final class Player(val rating: Elo, val kFactor: KFactor)

  final class Game(val points: Outcome.Points, val opponentRating: Elo)

  // 8.1.2 FIDE table
  val conversionTableFIDE: Map[Int, Float] = Map(
    0 -> 0.50f,
    1 -> 0.50f,
    2 -> 0.50f,
    3 -> 0.50f,
    4 -> 0.51f,
    5 -> 0.51f,
    6 -> 0.51f,
    7 -> 0.51f,
    8 -> 0.51f,
    9 -> 0.51f,
    10 -> 0.51f,
    11 -> 0.52f,
    12 -> 0.52f,
    13 -> 0.52f,
    14 -> 0.52f,
    15 -> 0.52f,
    16 -> 0.52f,
    17 -> 0.52f,
    18 -> 0.53f,
    19 -> 0.53f,
    20 -> 0.53f,
    21 -> 0.53f,
    22 -> 0.53f,
    23 -> 0.53f,
    24 -> 0.53f,
    25 -> 0.53f,
    26 -> 0.54f,
    27 -> 0.54f,
    28 -> 0.54f,
    29 -> 0.54f,
    30 -> 0.54f,
    31 -> 0.54f,
    32 -> 0.54f,
    33 -> 0.55f,
    34 -> 0.55f,
    35 -> 0.55f,
    36 -> 0.55f,
    37 -> 0.55f,
    38 -> 0.55f,
    39 -> 0.55f,
    40 -> 0.56f,
    41 -> 0.56f,
    42 -> 0.56f,
    43 -> 0.56f,
    44 -> 0.56f,
    45 -> 0.56f,
    46 -> 0.56f,
    47 -> 0.57f,
    48 -> 0.57f,
    49 -> 0.57f,
    50 -> 0.57f,
    51 -> 0.57f,
    52 -> 0.57f,
    53 -> 0.57f,
    54 -> 0.58f,
    55 -> 0.58f,
    56 -> 0.58f,
    57 -> 0.58f,
    58 -> 0.58f,
    59 -> 0.58f,
    60 -> 0.58f,
    61 -> 0.58f,
    62 -> 0.59f,
    63 -> 0.59f,
    64 -> 0.59f,
    65 -> 0.59f,
    66 -> 0.59f,
    67 -> 0.59f,
    68 -> 0.59f,
    69 -> 0.60f,
    70 -> 0.60f,
    71 -> 0.60f,
    72 -> 0.60f,
    73 -> 0.60f,
    74 -> 0.60f,
    75 -> 0.60f,
    76 -> 0.60f,
    77 -> 0.61f,
    78 -> 0.61f,
    79 -> 0.61f,
    80 -> 0.61f,
    81 -> 0.61f,
    82 -> 0.61f,
    83 -> 0.61f,
    84 -> 0.62f,
    85 -> 0.62f,
    86 -> 0.62f,
    87 -> 0.62f,
    88 -> 0.62f,
    89 -> 0.62f,
    90 -> 0.62f,
    91 -> 0.62f,
    92 -> 0.63f,
    93 -> 0.63f,
    94 -> 0.63f,
    95 -> 0.63f,
    96 -> 0.63f,
    97 -> 0.63f,
    98 -> 0.63f,
    99 -> 0.64f,
    100 -> 0.64f,
    101 -> 0.64f,
    102 -> 0.64f,
    103 -> 0.64f,
    104 -> 0.64f,
    105 -> 0.64f,
    106 -> 0.64f,
    107 -> 0.65f,
    108 -> 0.65f,
    109 -> 0.65f,
    110 -> 0.65f,
    111 -> 0.65f,
    112 -> 0.65f,
    113 -> 0.65f,
    114 -> 0.66f,
    115 -> 0.66f,
    116 -> 0.66f,
    117 -> 0.66f,
    118 -> 0.66f,
    119 -> 0.66f,
    120 -> 0.66f,
    121 -> 0.66f,
    122 -> 0.67f,
    123 -> 0.67f,
    124 -> 0.67f,
    125 -> 0.67f,
    126 -> 0.67f,
    127 -> 0.67f,
    128 -> 0.67f,
    129 -> 0.67f,
    130 -> 0.68f,
    131 -> 0.68f,
    132 -> 0.68f,
    133 -> 0.68f,
    134 -> 0.68f,
    135 -> 0.68f,
    136 -> 0.68f,
    137 -> 0.68f,
    138 -> 0.69f,
    139 -> 0.69f,
    140 -> 0.69f,
    141 -> 0.69f,
    142 -> 0.69f,
    143 -> 0.69f,
    144 -> 0.69f,
    145 -> 0.69f,
    146 -> 0.70f,
    147 -> 0.70f,
    148 -> 0.70f,
    149 -> 0.70f,
    150 -> 0.70f,
    151 -> 0.70f,
    152 -> 0.70f,
    153 -> 0.70f,
    154 -> 0.71f,
    155 -> 0.71f,
    156 -> 0.71f,
    157 -> 0.71f,
    158 -> 0.71f,
    159 -> 0.71f,
    160 -> 0.71f,
    161 -> 0.71f,
    162 -> 0.71f,
    163 -> 0.72f,
    164 -> 0.72f,
    165 -> 0.72f,
    166 -> 0.72f,
    167 -> 0.72f,
    168 -> 0.72f,
    169 -> 0.72f,
    170 -> 0.72f,
    171 -> 0.73f,
    172 -> 0.73f,
    173 -> 0.73f,
    174 -> 0.73f,
    175 -> 0.73f,
    176 -> 0.73f,
    177 -> 0.73f,
    178 -> 0.73f,
    179 -> 0.73f,
    180 -> 0.74f,
    181 -> 0.74f,
    182 -> 0.74f,
    183 -> 0.74f,
    184 -> 0.74f,
    185 -> 0.74f,
    186 -> 0.74f,
    187 -> 0.74f,
    188 -> 0.74f,
    189 -> 0.75f,
    190 -> 0.75f,
    191 -> 0.75f,
    192 -> 0.75f,
    193 -> 0.75f,
    194 -> 0.75f,
    195 -> 0.75f,
    196 -> 0.75f,
    197 -> 0.75f,
    198 -> 0.76f,
    199 -> 0.76f,
    200 -> 0.76f,
    201 -> 0.76f,
    202 -> 0.76f,
    203 -> 0.76f,
    204 -> 0.76f,
    205 -> 0.76f,
    206 -> 0.76f,
    207 -> 0.77f,
    208 -> 0.77f,
    209 -> 0.77f,
    210 -> 0.77f,
    211 -> 0.77f,
    212 -> 0.77f,
    213 -> 0.77f,
    214 -> 0.77f,
    215 -> 0.77f,
    216 -> 0.78f,
    217 -> 0.78f,
    218 -> 0.78f,
    219 -> 0.78f,
    220 -> 0.78f,
    221 -> 0.78f,
    222 -> 0.78f,
    223 -> 0.78f,
    224 -> 0.78f,
    225 -> 0.78f,
    226 -> 0.79f,
    227 -> 0.79f,
    228 -> 0.79f,
    229 -> 0.79f,
    230 -> 0.79f,
    231 -> 0.79f,
    232 -> 0.79f,
    233 -> 0.79f,
    234 -> 0.79f,
    235 -> 0.79f,
    236 -> 0.80f,
    237 -> 0.80f,
    238 -> 0.80f,
    239 -> 0.80f,
    240 -> 0.80f,
    241 -> 0.80f,
    242 -> 0.80f,
    243 -> 0.80f,
    244 -> 0.80f,
    245 -> 0.80f,
    246 -> 0.81f,
    247 -> 0.81f,
    248 -> 0.81f,
    249 -> 0.81f,
    250 -> 0.81f,
    251 -> 0.81f,
    252 -> 0.81f,
    253 -> 0.81f,
    254 -> 0.81f,
    255 -> 0.81f,
    256 -> 0.81f,
    257 -> 0.82f,
    258 -> 0.82f,
    259 -> 0.82f,
    260 -> 0.82f,
    261 -> 0.82f,
    262 -> 0.82f,
    263 -> 0.82f,
    264 -> 0.82f,
    265 -> 0.82f,
    266 -> 0.82f,
    267 -> 0.82f,
    268 -> 0.83f,
    269 -> 0.83f,
    270 -> 0.83f,
    271 -> 0.83f,
    272 -> 0.83f,
    273 -> 0.83f,
    274 -> 0.83f,
    275 -> 0.83f,
    276 -> 0.83f,
    277 -> 0.83f,
    278 -> 0.83f,
    279 -> 0.84f,
    280 -> 0.84f,
    281 -> 0.84f,
    282 -> 0.84f,
    283 -> 0.84f,
    284 -> 0.84f,
    285 -> 0.84f,
    286 -> 0.84f,
    287 -> 0.84f,
    288 -> 0.84f,
    289 -> 0.84f,
    290 -> 0.84f,
    291 -> 0.85f,
    292 -> 0.85f,
    293 -> 0.85f,
    294 -> 0.85f,
    295 -> 0.85f,
    296 -> 0.85f,
    297 -> 0.85f,
    298 -> 0.85f,
    299 -> 0.85f,
    300 -> 0.85f,
    301 -> 0.85f,
    302 -> 0.85f,
    303 -> 0.86f,
    304 -> 0.86f,
    305 -> 0.86f,
    306 -> 0.86f,
    307 -> 0.86f,
    308 -> 0.86f,
    309 -> 0.86f,
    310 -> 0.86f,
    311 -> 0.86f,
    312 -> 0.86f,
    313 -> 0.86f,
    314 -> 0.86f,
    315 -> 0.86f,
    316 -> 0.87f,
    317 -> 0.87f,
    318 -> 0.87f,
    319 -> 0.87f,
    320 -> 0.87f,
    321 -> 0.87f,
    322 -> 0.87f,
    323 -> 0.87f,
    324 -> 0.87f,
    325 -> 0.87f,
    326 -> 0.87f,
    327 -> 0.87f,
    328 -> 0.87f,
    329 -> 0.88f,
    330 -> 0.88f,
    331 -> 0.88f,
    332 -> 0.88f,
    333 -> 0.88f,
    334 -> 0.88f,
    335 -> 0.88f,
    336 -> 0.88f,
    337 -> 0.88f,
    338 -> 0.88f,
    339 -> 0.88f,
    340 -> 0.88f,
    341 -> 0.88f,
    342 -> 0.88f,
    343 -> 0.88f,
    344 -> 0.88f,
    345 -> 0.89f,
    346 -> 0.89f,
    347 -> 0.89f,
    348 -> 0.89f,
    349 -> 0.89f,
    350 -> 0.89f,
    351 -> 0.89f,
    352 -> 0.89f,
    353 -> 0.89f,
    354 -> 0.89f,
    355 -> 0.89f,
    356 -> 0.89f,
    357 -> 0.89f,
    358 -> 0.90f,
    359 -> 0.90f,
    360 -> 0.90f,
    361 -> 0.90f,
    362 -> 0.90f,
    363 -> 0.90f,
    364 -> 0.90f,
    365 -> 0.90f,
    366 -> 0.90f,
    367 -> 0.90f,
    368 -> 0.90f,
    369 -> 0.90f,
    370 -> 0.90f,
    371 -> 0.90f,
    372 -> 0.90f,
    373 -> 0.90f,
    374 -> 0.90f,
    375 -> 0.91f,
    376 -> 0.91f,
    377 -> 0.91f,
    378 -> 0.91f,
    379 -> 0.91f,
    380 -> 0.91f,
    381 -> 0.91f,
    382 -> 0.91f,
    383 -> 0.91f,
    384 -> 0.91f,
    385 -> 0.91f,
    386 -> 0.91f,
    387 -> 0.91f,
    388 -> 0.91f,
    389 -> 0.91f,
    390 -> 0.91f,
    391 -> 0.91f
  )
