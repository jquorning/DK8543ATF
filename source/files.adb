--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Directories;
with Ada.Text_IO;

with Setup;
with Database;

package body Files is

   ---------------------------------------------------------------------------
   --  Iterate througt Directory adding valid databases to Collection
   --
   procedure Append_Collections (Collection : in out Collection_List;
                                 Directory  : in     String)
   is
      use Ada.Directories;

      DB_Only : constant String := "*." & Setup.Database_Extension;

      Filter  : constant Filter_Type :=
        (Ordinary_File => True, others => False);

      DB_Search  : Search_Type;
      File_Entry : Directory_Entry_Type;

   begin

      if not Exists (Directory) then
         return;
      end if;

      Start_Search (DB_Search,
                    Directory => Directory,
                    Pattern   => DB_Only,
                    Filter    => Filter);
      while
        More_Entries (DB_Search)
      loop

         Get_Next_Entry (DB_Search, File_Entry);

         if Database.Is_Valid (Full_Name (File_Entry)) then
            Collection.Append (To_Unbounded_String (Full_Name (File_Entry)));
            Ada.Text_IO.Put_Line ("Adding => " & Full_Name (File_Entry));
         end if;

      end loop;
      End_Search (DB_Search);

   end Append_Collections;


end Files;
