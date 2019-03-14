--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with GNATCOLL.Readline;

with Setup;
with Readline_Helper;

package body Interactive is

   History_File : constant String := Setup.Program_Name & ".history";

   procedure Initialize is
   begin
      GNATCOLL.Readline.Initialize
        (Appname      => Setup.Program_Name,
         History_File => History_File,
         Completer    => Readline_Helper.Completer'Access);
   end Initialize;


   procedure Finalize is
   begin
      GNATCOLL.Readline.Finalize (History_File);
   end Finalize;


   function Get_Line return String is
   begin
      return GNATCOLL.Readline.Get_Line ("TODO$ ");
   end Get_Line;


end Interactive;

