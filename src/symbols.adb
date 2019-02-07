--
--  UTF-8 symbols
--

--  with Ada.Text_IO;

package body Symbols is

--     package Hex_IO is
--        new Ada.Text_IO.Integer_IO (Integer);

--     subtype Image_Type is String (1 .. 2);
--     function To_Hex (I : Integer) return Image_Type;
--     function To_Hex (I : Integer) return Image_Type is
--        Image : String (1 .. 10);
--     begin
--        Hex_IO.Put (Image, I, Base => 16);
--        if Image (Image'Last - 2) = '#' then
--           Image (Image'Last - 2) := '0';
--        end if;
--        return Image (Image'Last - 2 .. Image'Last - 1);
--     end To_Hex;

--   function HTML_Image (Symbol : Symbol_Type) return String is
--   begin
--      return "&#x"
--        & To_Hex (Character'Pos (Symbol (1)))
--        & To_Hex (Character'Pos (Symbol (2))) & ";";
--   end HTML_Image;

   type Ss_Type is
      record
         UTF8 : String (1 .. 3);
         HTML : String (1 .. 8);
      end record;

   ESC : constant Character := Character'Val (16#E2#);

   List : constant array (Symbol_Type) of Ss_Type :=
     (Space               =>  ("   ", "&#x0020;"),
      Plus_Sign           =>  (" + ", "&#x002B;"),
      Minus_Sign          =>  (" - ", "&#x2212;"),
      Multiplication_Sign =>  (" * ", "&#x00D7;"),
      Division_Sign       =>  (" / ", "&#x00F7;"),

      Black_Star =>
        ((ESC & Character'Val (16#98#) & Character'Val (16#85#)), "&#x2605;"),
      White_Star =>
        ((ESC & Character'Val (16#98#) & Character'Val (16#86#)), "&#x2606;"),
      Black_Right_Pointing_Index =>
        ((ESC & Character'Val (16#98#) & Character'Val (16#9B#)), "&#x261B;"),
      White_Right_Pointing_Index =>
        ((ESC & Character'Val (16#98#) & Character'Val (16#9E#)), "&#x261E;"),

      Check_Mark =>
        ((ESC & Character'Val (16#9C#) & Character'Val (16#93#)), "&#x2713;"),
      Heavy_Check_Mark =>
        ((ESC & Character'Val (16#9C#) & Character'Val (16#94#)), "&#x2714;"),
      Multiplication_X =>
        ((ESC & Character'Val (16#9C#) & Character'Val (16#95#)), "&#x2715;"),
      Heavy_Multiplication_X =>
        ((ESC & Character'Val (16#9C#) & Character'Val (16#96#)), "&#x2716;"),
      Ballot_X =>
        ((ESC & Character'Val (16#9C#) & Character'Val (16#97#)), "&#x2717;"),
      Heavy_Ballot_X =>
        ((ESC & Character'Val (16#9C#) & Character'Val (16#98#)), "&#x2718;"),
      Medium_Left_Parenthesis_Ornament             =>  (" ( ", "&#x2768;"),
      Medium_Right_Parenthesis_Ornament            =>  (" ) ", "&#x2769;"),
      Medium_Flattened_Left_Parenthesis_Ornament   =>  (" ( ", "&#x276A;"),
      Medium_Flattened_Right_Parenthesis_Ornament  =>  (" ) ", "&#x276B;"),
      Medium_Left_Pointing_Angle_Bracket_Ornament  =>  (" < ", "&#x276C;"),
      Medium_Right_Pointing_Angle_Bracket_Ornament =>  (" > ", "&#x276D;"));

   function UTF8 (Symbol : in Symbol_Type)
                 return String
   is
      (List (Symbol).UTF8);


   function HTML (Symbol : in Symbol_Type)
                 return String
   is
      (List (Symbol).HTML);


end Symbols;

