--
--  Parser
--

package Parser is

   procedure Parse_Input (Input : in String);

   function Get_Last_Command return String;

   function Exit_Program return Boolean;

end Parser;
