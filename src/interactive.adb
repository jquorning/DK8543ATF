--
--  Interactive
--  TODO terminal user interface
--

with Ada.Text_IO;

with GNATCOLL.Readline;

with Readline_Helper;

package body Interactive is

   History_File : constant String := "todo.history";
--   Symbol_File  : constant String := "todo.symbols";

   procedure Initialize is
   begin
      Ada.Text_IO.Put_Line ("Interactive.Initialize");
      GNATCOLL.Readline.Initialize
        (Appname      => "todo",
         History_File => History_File,
         Completer    => Readline_Helper.Completer'Access);

--      Symbols.Load (Symbol_File);

--        declare
--           Line : constant String := Get_Line (Prompt => "PROMPT: ");
--        begin
--           Ada.Text_IO.Put_Line ("X#" & Line & "#");
--           if Line (1 .. 5) = ".load" then
--              declare
--                 Tree : Parsers.Tree_Type := Parsers.New_Tree;
--              begin
--                 Parsers.Parse_File
--                  (Tree, File_Name => Line (7 .. Line'Last));
--              end;
--           else
--              Symbols.Append (Symbols.K_Procedure, Line);
--           end if;
--        end;
   end Initialize;


   procedure Finalize is
   begin
--      Symbols.Save (Symbol_File);

      GNATCOLL.Readline.Finalize (History_File);
   end Finalize;


   function Get_Line return String is
   begin
      return GNATCOLL.Readline.Get_Line ("TODO$ ");
   end Get_Line;


end Interactive;

