--
--
--

with GNATCOLL.Readline;

package Readline_Helper is

   function Completer
     (Full_Line   : String;
      Text        : String;
      Start, Last : Integer)
     return GNATCOLL.Readline.Possible_Completions;

end Readline_Helper;

