--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package Parser is

   procedure Parse_Input (Input : in String);

   function Get_Last_Command return String;

   function Exit_Program return Boolean;

end Parser;
