{
module Lexer where

import Model
}

%wrapper "basic"

tokens :-
  _         { const  }
| .         { const  }
  ,         { const  }
  go        { const  }
  take      { const  }
  mark      { const  }
  nothing   { const  }
  turn      { const  }
  case      { const  }
  of        { const  }
  end       { const  }
  left      { const  }
  right     { const  }
  front     { const  }
  ;         { const  }
  Empty     { const Empty }
  Lambda    { const Lambda }
  Debris    { const Debris }
  Asteroid  { const Asteroid }
  Boundary  { const Boundary }
  _         { const  }
  Ident     { const  }
