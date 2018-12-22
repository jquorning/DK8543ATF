--
--  UTF-8 symbols
--

package Symbols is

   ESC : constant Character := Character'Val (16#E2#);

   subtype Symbol_Type is String (1 .. 3);
   Black_Star : constant Symbol_Type :=
     ESC & Character'Val (16#98#) & Character'Val (16#85#);
   White_Star : constant Symbol_Type :=
     ESC & Character'Val (16#98#) & Character'Val (16#86#);
   White_Right_Pointing_Index : constant Symbol_Type :=
     ESC & Character'Val (16#98#) & Character'Val (16#9E#);

   Check_Mark : constant Symbol_Type :=
     ESC & Character'Val (16#9C#) & Character'Val (16#93#);
   Heavy_Check_Mark : constant Symbol_Type :=
     ESC & Character'Val (16#9C#) & Character'Val (16#94#);
   Multiplication_X : constant Symbol_Type :=
     ESC & Character'Val (16#9C#) & Character'Val (16#95#);
   Heavy_Multiplication_X : constant Symbol_Type :=
     ESC & Character'Val (16#9C#) & Character'Val (16#96#);
   Ballot_X : constant Symbol_Type :=
     ESC & Character'Val (16#9C#) & Character'Val (16#97#);
   Heavy_Ballot_X : constant Symbol_Type :=
     ESC & Character'Val (16#9C#) & Character'Val (16#98#);

   procedure Dummy;

end Symbols;

