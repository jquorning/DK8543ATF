--
--  Database
--

with SQLite;

package Database is

   procedure Open;

private

   DB : SQLite.Data_Base;
   --  Used by child pacakge.

end Database;
