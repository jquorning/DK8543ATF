--
--  Database
--

with SQLite;

package Database is

   procedure Open;

   function Is_Valid (File_Name : in String)
                     return Boolean;
   --  True when File_Name designates valid database.

private

   DB : SQLite.Data_Base;
   --  Used by child pacakge.

end Database;
