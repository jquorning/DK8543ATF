--
--
--

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Calendar;

with Database.Events;
with Commands;
with Terminal_IO;

package body Parser is

   Prompt_Text  : constant String := "TODO ";
   Run_Program  : Boolean         := True;
   Last_Command : Ada.Strings.Unbounded.Unbounded_String;

   procedure Put_Prompt is
   begin
      Ada.Text_IO.Put (Prompt_Text);
   end Put_Prompt;

   function Get_Input return String is
   begin
      return Ada.Text_IO.Get_Line;
   end Get_Input;

   use Ada.Strings.Unbounded;
   function "-" (Source : String) return Unbounded_String
     renames To_Unbounded_String;

   type Help_Line is
      record
         Command : Unbounded_String;
         Comment : Unbounded_String;
      end record;

   Help_Lines : constant array (Positive range <>) of Help_Line :=
     ((-"help",  -"Print this help text"),
      (-"quit",  -"Quit program"),
      (-"view",  -"Show current list"),
      (-"lists", -"Show all list"),
      (-"set list LIST", -"Set LIST to current"),
      (-"set job JOB",   -"Set JOB to current"),
      (-"show list",
        -"Show all jobs in current list"),
      (-"show job",      -"Show jobs details"),
      (-"add job TITLE", -"Add job to current list"),
      (-"add list NAME", -"Add list to database"),
      (-"split serial JOB COUNT",
       -"Split job into count serial jobs"),
      (-"split parallel JOB COUNT ",
       -"Split job into count parallel jobs"),
      (-"move LIST",
       -"Move current job to other list"),
      (-"trans LIST",
       -"Transfer current job to other list"),
      (-"event KIND", -"Add event to current job"));

   function Web_Help return String is
      S : Unbounded_String;
   begin
      Append (S, "<table>");
      for Line of Help_Lines loop
         Append (S, "<tr><td>"
                   & Line.Command
                   & "</td><td>"
                   & Line.Comment
                   & "</td></tr>");
      end loop;
      Append (S, "</table>");
      return To_String (S);
   end Web_Help;

   procedure Put_Help is
      use Ada.Text_IO;
   begin
      for Line of Help_Lines loop
         Set_Col (1);
         Put (To_String (Line.Command));
         Set_Col (33);
         Put_Line (To_String (Line.Comment));
      end loop;
   end Put_Help;

   procedure Put_Banner is
      use Ada.Text_IO;
   begin
      Put_Line ("TODO List program");
      Put_Line ("=================");
      Put_Line ("type help to show help text");
   end Put_Banner;


   procedure Put_Error;
   procedure Set (Command : in String);
   procedure Show (Command : in String);
   procedure Add (Command : in String);
   procedure Event (Command : in String);
   procedure Transfer (Command : in String);


   procedure Put_Error is
   begin
      Ada.Text_IO.Put_Line ("Unknown command: '" & Get_Last_Command & "'");
   end Put_Error;

   function Exit_Program return Boolean is
   begin
      return not Run_Program;
   end Exit_Program;


   procedure Set (Command : in String) is
      Space_Pos : constant Natural := Ada.Strings.Fixed.Index (Command, " ");
      First     : constant String
        := (if Space_Pos = 0 then Command
        else Command (Command'First .. Space_Pos - 1));
      Rest     : constant String
        := (if Space_Pos = 0 then ""
        else Ada.Strings.Fixed.Trim (Command (Space_Pos .. Command'Last),
                                     Ada.Strings.Both));
   begin
      if First = "list" then
         Database.Lists.Current := Database.Lookup_List (Rest);
      elsif First = "job" then
         Database.Jobs.Current  := Database.Lookup_Job (Rest);
      else
         raise Constraint_Error;
      end if;
   end Set;

   procedure Show (Command : in String) is
      Space_Pos : constant Natural := Ada.Strings.Fixed.Index (Command, " ");
      First     : constant String
        := (if Space_Pos = 0 then Command
        else Command (Command'First .. Space_Pos - 1));
--        Rest     : constant String
--          := (if Space_Pos = 0 then ""
--          else Command (Space_Pos .. Command'Last));
   begin
      if First = "list" then
         Database.Show_List (Database.Lists.Current);
      elsif First = "job" then
         Database.Show_Job (Database.Jobs.Current);
      else
         raise Constraint_Error;
      end if;
   end Show;


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
         Commands.Create_Job
           (Database.Get_Job_Id,
            Ada.Strings.Fixed.Trim (Rest, Ada.Strings.Both),
            Database.Lists.Current);
      elsif First = "list" then
         Database.Create_List
           (Ada.Strings.Fixed.Trim (Rest, Ada.Strings.Both));
      else
         raise Constraint_Error;
      end if;
   end Add;


   procedure Event (Command : in String) is
      pragma Unreferenced (Command);
      --        Space_Pos : constant Natural
      --  := Ada.Strings.Fixed.Index (Command, " ");
--        First     : constant String
--          := (if Space_Pos = 0 then Command
--          else Command (Command'First .. Space_Pos - 1));
--        Rest     : constant String
--          := (if Space_Pos = 0 then ""
--          else Command (Space_Pos .. Command'Last));
      Id : Database.Events.Event_Id;
      pragma Unreferenced (Id);
   begin
      Database.Events.Add_Event (Database.Jobs.Current,
                                 Ada.Calendar.Clock,
                                 Database.Events.Deadline,
                                 Id);
   end Event;

   procedure Transfer (Command : in String) is
      use Database;
   begin
      Transfer (Job     => Jobs.Current,
                To_List => Lookup_List (Command));
   end Transfer;

   procedure Parse_Input (Input : in String) is
      Space_Pos : constant Natural := Ada.Strings.Fixed.Index (Input, " ");
      First     : constant String
        := (if Space_Pos = 0 then Input
        else Input (Input'First .. Space_Pos - 1));
      Rest     : constant String
        := (if Space_Pos = 0 then ""
        else Ada.Strings.Fixed.Trim (Input (Space_Pos .. Input'Last),
                                     Ada.Strings.Both));
   begin
      Last_Command := Ada.Strings.Unbounded.To_Unbounded_String (Input);
      if First = "" then
         null;
      elsif First = "quit" then
         Run_Program := False;
      elsif First = "help" then
         Put_Help;
      elsif First = "view" then
         Database.Get_Jobs (Database.Jobs,
                            List => Database.Lists.Current);
         Terminal_IO.Put_Jobs (Database.Jobs);
      elsif First = "lists" then
         Database.Get_Lists (Database.Lists);
         Terminal_IO.Put_Lists (Database.Lists);
      elsif First = "set" then
         Set (Rest);
      elsif First = "show" then
         Show (Rest);
      elsif First = "add" then
         Add (Rest);
      elsif First = "split" then
         raise Program_Error;
      elsif First = "move" then
         Transfer (Rest);
      elsif First = "trans" then
         Transfer (Rest);
      elsif First = "event" then
         Event (Rest);
      else
         Put_Error;
      end if;

   exception
      when Constraint_Error =>
         Put_Error;

   end Parse_Input;

   function Get_Last_Command return String is
   begin
      return Ada.Strings.Unbounded.To_String (Last_Command);
   end Get_Last_Command;

end Parser;
