--
--  Parser body
--

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;

with Database.Jobs;
with Database.Events;
with Commands;
with Terminal_IO;
with Navigate;
with CSV_IO;
with Types;

package body Parser is

   Run_Program  : Boolean := True;

   Last_Command : Ada.Strings.Unbounded.Unbounded_String;


   procedure Set (Command : in String);
   procedure Add (Command : in String);
   procedure Event (Command : in String);
   procedure Transfer (Command : in String);


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
      Lookup_Success : Boolean;
   begin
      if First = "job" then
         declare
            New_Current_Job : Types.Job_Id;
         begin
            Navigate.Lookup_Job (Text    => Rest,
                                 Job     => New_Current_Job,
                                 Success => Lookup_Success);
            if Lookup_Success then
               Commands.Set_Current_Job (New_Current_Job);
            else
               raise Constraint_Error with "Could not lookup Text";
            end if;
         end;
      else
         raise Constraint_Error with "Set with unknown First";
      end if;
   end Set;


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
           (Database.Jobs.Get_New_Job_Id,
            Ada.Strings.Fixed.Trim (Rest, Ada.Strings.Both),
            Parent => Database.Jobs.Get_Current_Job);
--      elsif First = "list" then
--         Database.Create_List
--           (Ada.Strings.Fixed.Trim (Rest, Ada.Strings.Both));
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
      Database.Events.Add_Event (Database.Jobs.Get_Current_Job,
                                 Ada.Calendar.Clock,
                                 Database.Events.Done,
                                 Id);
   end Event;


   procedure Transfer (Command : in String) is
      use Database.Jobs;
      Success   : Boolean;
      To_Parent : Types.Job_Id;
   begin
      Navigate.Lookup_Job (Command, To_Parent, Success);
      Transfer (Job       => Get_Current_Job,
                To_Parent => To_Parent);
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
         Terminal_IO.Put_Help;
      elsif
        First = "view" or
        First = "ls"
      then
         Terminal_IO.Put_Jobs;
--         Terminal_IO.Put_Jobs
--           (Database.Jobs.Get_Jobs (Database.Jobs.Get_Current_Job));
      elsif
        First = "top" or
        First = "cd"
      then
         Terminal_IO.Put_Jobs;
--         Terminal_IO.Put_Jobs
--           (Database.Jobs.Get_Jobs (Database.Jobs.Top_Level));
      elsif First = "set" then
         Set (Rest);
      elsif
        First = "show" or
        First = "cat"
      then
         Terminal_IO.Show_Job (Database.Jobs.Get_Current_Job);
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
      elsif First = "export" then
         CSV_IO.Export ("what-to-do.csv");
      else
         Terminal_IO.Put_Error ("Unknown command: '" & Get_Last_Command & "'");
      end if;

   exception

      when Constraint_Error =>
         Terminal_IO.Put_Error ("Constraint_Error in parser");

      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         Terminal_IO.Put_Error (" Other exception raised");

   end Parse_Input;


   function Get_Last_Command return String is
   begin
      return Ada.Strings.Unbounded.To_String (Last_Command);
   end Get_Last_Command;


end Parser;
