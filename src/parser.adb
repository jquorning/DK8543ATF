--
--
--

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Calendar;

with Database.Events;
with Commands;

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
      Put_Line ("set list <list>              - Set list to current");
      Put_Line ("set job <job>                - Set job to current");
      Put_Line ("show list                    " &
                  "- Show all jobs in current list");
      Put_Line ("show job                     - Show jobs details");
      Put_Line ("add job <title>              - Add job to current list");
      Put_Line ("add list <name>              - Add list to database");
      Put_Line ("split serial <job> <count>   " &
                  "- Split job into count serial jobs");
      Put_Line ("split parallel <job> <count> " &
                  "- Split job into count parallel jobs");
      Put_Line ("move <list>                  " &
                  "- Move current job to other list");
      Put_Line ("trans <list>                 " &
                  "- transfer current job to other list");
      Put_Line ("event <kind>                 - Add event to current job");
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
      Ada.Text_IO.Put_Line ("Unknown command");
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
      if First = "" then
         null;
      elsif First = "quit" then
         Run_Program := False;
      elsif First = "help" then
         Put_Help;
      elsif First = "view" then
         Database.Get_Jobs (Database.Jobs);
         Database.Put_Jobs (Database.Jobs);
      elsif First = "lists" then
         Database.Get_Lists (Database.Lists);
         Database.Put_Lists (Database.Lists);
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

end Parser;
