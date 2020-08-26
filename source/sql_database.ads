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

package SQL_Database is

   procedure Open;
   --  Open database.
   --  Raise Program_Error on fail.

   function Is_Valid
     (File_Name : in String)
      return Boolean;
   --  True when File_Name designates valid database.

end SQL_Database;
