--
--
--

with Ada.Text_IO;
with Ada.Strings.Fixed;

with Database;

package body Parser is

   Prompt_Text : constant String := "TODO ";
   Run_Program : Boolean         := True;

   procedure Put_Prompt is
   begin
      Ada.Text_IO.Put (Prompt_Text);
   end Put_Prompt;

   function Get_Input return String is
   begin
      return Ada.Text_IO.Get_Line;
   end Get_Input;

   procedure Put_Help is
      use Ada.Text_IO;
   begin
      Put_Line ("help        - Print this help text      " &
                  "quit        - Quit program");
      Put_Line ("view        - Show current list         " &
                  "lists       - Show all list");
      Put_Line ("add job <title>              - Add job to current list");
      Put_Line ("add list <name>              - Add list to database");
      Put_Line ("split serial <job> <count>   " &
                  "- Split job into count serial jobs");
      Put_Line ("split parallel <job> <count> " &
                  "- Split job into count parallel jobs");
      Put_Line ("trans <job> <list>           - Transfer job to other list");
   end Put_Help;

   procedure Put_Banner is
      use Ada.Text_IO;
   begin
      Put_Line ("TODO List program");
      Put_Line ("=================");
      Put_Line ("type help to show help text");
   end Put_Banner;

   procedure Put_Error is
   begin
      Ada.Text_IO.Put_Line ("Unknown command");
   end Put_Error;

   function Exit_Program return Boolean is
   begin
      return not Run_Program;
   end Exit_Program;

   procedure Add (Command : in String) is
      Space_Pos : constant Natural := Ada.Strings.Fixed.Index (Command, " ");
      First     : constant String
        := (if Space_Pos = 0 then Command
        else Command (Command'First .. Space_Pos - 1));
      Rest     : constant String
        := (if Space_Pos = 0 then ""
        else Command (Space_Pos .. Command'Last));
   begin
      if First = "job" then
         Database.Create_Job
           (Database.Get_Job_Id,
            Ada.Strings.Fixed.Trim (Rest, Ada.Strings.Both));
      elsif First = "list" then
         Database.Create_List
           (Ada.Strings.Fixed.Trim (Rest, Ada.Strings.Both));
      else
         raise Constraint_Error;
      end if;
   end Add;

   procedure Parse_Input (Input : in String) is
      Space_Pos : constant Natural := Ada.Strings.Fixed.Index (Input, " ");
      First     : constant String
        := (if Space_Pos = 0 then Input
        else Input (Input'First .. Space_Pos - 1));
      Rest     : constant String
        := (if Space_Pos = 0 then ""
        else Input (Space_Pos .. Input'Last));
   begin
      if First = "" then
         null;
      elsif First = "quit" then
         Run_Program := False;
      elsif First = "help" then
         Put_Help;
      elsif First = "view" then
         Database.Put_Jobs;
      elsif First = "lists" then
         Database.Put_Lists;
      elsif First = "add" then
         Add (Ada.Strings.Fixed.Trim (Rest, Ada.Strings.Both));
      elsif First = "split" then
         raise Program_Error;
      elsif First = "trans" then
         raise Program_Error;
      else
         Put_Error;
      end if;

   exception
      when Constraint_Error =>
         Put_Error;

   end Parse_Input;

end Parser;
