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

   procedure Dummy is null;

end Symbols;

