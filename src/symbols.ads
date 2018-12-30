--
--  UTF-8 symbols
--

package Symbols is


   type Symbol_Type is
     (Black_Star,
      White_Star,
      Black_Right_Pointing_Index,
      White_Right_Pointing_Index,

      Check_Mark,
      Heavy_Check_Mark,
      Multiplication_X,
      Heavy_Multiplication_X,
      Ballot_X,
      Heavy_Ballot_X);

   function UTF8 (Symbol : in Symbol_Type)
                 return String;

   function HTML (Symbol : in Symbol_Type)
                 return String;

end Symbols;

