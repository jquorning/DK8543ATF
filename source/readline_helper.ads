--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with GNATCOLL.Readline;

package Readline_Helper is

   function Completer
     (Full_Line   : String;
      Text        : String;
      Start, Last : Integer)
     return GNATCOLL.Readline.Possible_Completions;

end Readline_Helper;

