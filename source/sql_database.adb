pragma License (Restricted);
--
--  Copyright (C) 2020 Jesper Quorning All Rights Reserved.
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
with Database;
with SQLite;

package body SQL_Database is

--   Default_Database   : constant String :=
--     Setup.Program_Name & "." & Setup.Database_Extension;

   use Ada.Strings.Unbounded;

   function "+" (Source : String)
                return Unbounded_String
     renames To_Unbounded_String;

   ----------
   -- Open --
   ----------

   procedure Open
   is
      use Ada.Text_IO;

      Success : Boolean := False;

      procedure Try_Open (File_Name :     String;
                          Success   : out Boolean);
      --  Try to open the SQLite database using File_Name.

      procedure Try_Open (File_Name :     String;
                          Success   : out Boolean)
      is
         use SQLite;
      begin
         Success     := False;  --  Pessimism - result when no exception
         Database.DB := SQLite.Open (File_Name => File_Name,
                                     Flags     => READWRITE or FULLMUTEX);
         Success     := True;
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

      File_Name : constant String
        := Setup.Database_Others & "." & Setup.Database_Extension;

   begin
      for Path of Paths loop
         declare
            Full_Path_Name : constant String
              := To_String (Path) & File_Name;
         begin
            Try_Open (Full_Path_Name, Success);
            if Success then
               return;
            end if;
         end;
      end loop;

      raise Program_Error
        with "Could not open database file '" & File_Name & "'";

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


end SQL_Database;
