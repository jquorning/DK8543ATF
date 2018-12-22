--
--  UTF-8 symbols
--

package Symbols is

   subtype Symbol_Type is String (1 .. 3);
   Black_Star : constant Symbol_Type :=
     Character'Val (16#E2#) & Character'Val (16#98#) & Character'Val (16#85#);
   White_Star : constant Symbol_Type :=
     Character'Val (16#E2#) & Character'Val (16#98#) & Character'Val (16#86#);

   function HTML_Image (Symbol : Symbol_Type) return String;

end Symbols;

