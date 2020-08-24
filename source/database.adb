--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Ada.Environment_Variables;

with Setup;

package body Database is

--   Default_Database   : constant String :=
--     Setup.Program_Name & "." & Setup.Database_Extension;

   use Ada.Strings.Unbounded;

   function "+" (Source : String)
                return Unbounded_String
     renames To_Unbounded_String;


   procedure Open
   is
      use Ada.Text_IO;

      Success : Boolean := False;

      procedure Try_Open (File_Name :     String;
                          Success   : out Boolean);

      procedure Try_Open (File_Name :     String;
                          Success   : out Boolean)
      is
         use SQLite; -- , Ada.IO_Exceptions;
      begin
         Success := True;  --  Optimism - result when no exception
         DB := SQLite.Open (File_Name => File_Name,
                            Flags     => READWRITE or FULLMUTEX);
      exception
         when Use_Error =>   --  Could not open database file
            Success := False;
      end Try_Open;


      package Env renames Ada.Environment_Variables;


      Home  : constant String
        := (if Env.Exists ("HOME") then Env.Value ("HOME") else "");

      Paths : constant array (Positive range <>) of Unbounded_String :=
          (+"./",             --  & Default_Database,
           +Home & "/etc/",   --      & Default_Database,
           +"/etc/",          --             & Default_Database,
           +Home & "/.");     --         & Default_Database);  --  Hidden

   begin
      for Path of Paths loop
         declare
            Full_Path_Name : constant String
              := To_String (Path) & "default" & "." & Setup.Database_Extension;
         begin
            Try_Open (Full_Path_Name, Success);
            if Success then
               return;
            end if;
         end;
      end loop;
      raise Program_Error with "Could not open database file";
   end Open;

   ---------------------------------------------------------------------------

   function Is_Valid (File_Name : in String)
                     return Boolean
   is
      DB : SQLite.Data_Base;
      pragma Unreferenced (DB);
   begin
      DB := SQLite.Open (File_Name => File_Name,
                         Flags     => SQLite.READONLY);
      return True;
   exception

      when Ada.IO_Exceptions.Use_Error =>
         return False;

   end Is_Valid;
   --  True when File_Name designates valid database.


end Database;
