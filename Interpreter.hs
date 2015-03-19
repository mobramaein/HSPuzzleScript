import qualified Data.Vector as V
--Keywords --
data Direction = Absolute AbsoluteDir | Relative RelativeDir | NoDir deriving (Eq, Show)
data RuleKeyWord = Late | Horizontal | Vertical | None deriving (Eq,Show)
data RelativeDir = Up| Down | Rgt | Lft | Perpendicular | Moving | Stationary | Parallel | NoOp deriving (Eq,Show)
data AbsoluteDir = AUp| ADown | ARgt | ALft | ANo | AEllipses | ANoOp deriving (Eq,Show)
data LogicWord = All | NoNe | On | Some deriving (Eq, Show)
data SectionNames = Objects| Legend | Sounds | CollisionLayers | Rules [Rule] | WinConditions [WinCondition] | Levels deriving(Eq, Show)
data CommandWords = Sfx0 |Sfx1 |Sfx2 |Sfx3 |Sfx4 |Sfx5 |Sfx6 |Sfx7 |Sfx8 |Sfx9 |Sfx10 | Cancel| Checkpoint | Restart |Win |Message | Again  deriving(Eq,Show)

-- Internal Data Types --

--String for name, list of Strings for Sprite -
data GameObject = GameObj String [String] deriving (Eq,Show)

--This is the type for a game rule--
data Rule = Obj String |
			Prec Direction Rule Direction Rule |
			Post Direction Rule Direction Rule |
			GameRule RuleKeyWord Rule Rule
			deriving (Eq,Show)

--This is the type for a Winning condition-
--There are games with no winning conditions.
data WinCondition = WinC LogicWord GameObject LogicWord GameObject | WinCS LogicWord GameObject deriving(Eq,Show)

--This is found in the legend section.
-- Format is $CHAR$ = {ObjectRef}
data LevelTokens = LevelToke String GameObject

data State = Stat [GameObject]
evalRule :: State -> Rule -> State
evalRule

main :: IO()
main =
		let
		player = Obj "Player"
		trail =  Obj "Trail"
		in
		do
		print $ GameRule None (Prec (Relative Rgt) player NoDir trail) (Post NoDir trail NoDir player)
