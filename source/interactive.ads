--
--  Interactive
--  TODO terminal user interface
--

--  with Ada.Text_IO;

--  with GNATCOLL.Readline;

--  with Helper;
--  Wit h Symbols;
--  with Parsers;

package Interactive is

   procedure Initialize;

   procedure Finalize;

   function Get_Line return String;

end Interactive;
