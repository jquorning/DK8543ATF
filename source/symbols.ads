--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package Symbols is


   type Symbol_Type is
     (Space,
      Plus_Sign,
      Minus_Sign,
      Multiplication_Sign,
      Division_Sign,
      Black_Star,
      White_Star,
      Black_Right_Pointing_Index,
      White_Right_Pointing_Index,

      Check_Mark,
      Heavy_Check_Mark,
      Multiplication_X,
      Heavy_Multiplication_X,
      Ballot_X,
      Heavy_Ballot_X,
      Medium_Left_Parenthesis_Ornament,
      Medium_Right_Parenthesis_Ornament,
      Medium_Flattened_Left_Parenthesis_Ornament,
      Medium_Flattened_Right_Parenthesis_Ornament,
      Medium_Left_Pointing_Angle_Bracket_Ornament,
      Medium_Right_Pointing_Angle_Bracket_Ornament);

   function UTF8 (Symbol : in Symbol_Type)
                 return String;

   function HTML (Symbol : in Symbol_Type)
                 return String;

end Symbols;

