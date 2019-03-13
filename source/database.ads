--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
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
