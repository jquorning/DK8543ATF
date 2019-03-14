--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package Setup is

   function Program_Name      return String;
   function Program_Version   return String;
   function Build_ISO8601_UTC return String;
   function Uname_M return String;
   function Uname_N return String;
   function Uname_P return String;
   function Uname_R return String;
   function Uname_S return String;

   Database_Extension : constant String := "wedonu";

   type Configuration is null record;

end Setup;
