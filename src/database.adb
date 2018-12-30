--
--  Database body
--

with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Ada.Environment_Variables;

package body Database is

   use Ada.Strings.Unbounded;

   function "+" (Source : String)
                return Unbounded_String
     renames To_Unbounded_String;

   Database_File_Name : constant String := "todo.db";

   procedure Open is
      Success : Boolean := False;

      procedure Try_Open (File_Name :     String;
                          Success   : out Boolean);

      procedure Try_Open (File_Name :     String;
                          Success   : out Boolean)
      is
         use SQLite, Ada.IO_Exceptions;
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

      Paths : constant array (Positive range <>) of Unbounded_String
          := (+"./", +Home & "/etc/", +"/etc/", +Home & "/Work/etc/");


   begin
      for Path of Paths loop

         declare
            Full_Path_Name : constant String
              := To_String (Path) & Database_File_Name;
         begin
            Try_Open (Full_Path_Name, Success);
            if Success then
               return;
            end if;
         end;
      end loop;
      raise Program_Error with "Could not open database file";
   end Open;


end Database;
