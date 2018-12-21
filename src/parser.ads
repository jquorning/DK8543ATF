--
--
--

package Parser is

   procedure Put_Prompt;

   function Get_Input return String;

   function Web_Help return String;
   procedure Put_Help;

   procedure Put_Banner;

   procedure Parse_Input (Input : in String);

   function Exit_Program return Boolean;

end Parser;
