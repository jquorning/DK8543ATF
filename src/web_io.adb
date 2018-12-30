--
--
--

with Ada.Strings.Unbounded;

with Database.Events;
with Commands;

package body Web_IO is


   function Help_Image return HTML_String is
      use Ada.Strings.Unbounded;
      S : Unbounded_String;
   begin
      Append (S, "<table>");
      for Line of Commands.Help_Lines loop
         Append (S, "<tr><td>"
                   & Line.Command
                   & "</td><td>"
                   & Line.Comment
                   & "</td></tr>");
      end loop;
      Append (S, "</table>");
      return To_String (S);
   end Help_Image;


   function Jobs_Image (Jobs : in Database.Jobs.Job_Set)
                       return String
   is
      use Ada.Strings.Unbounded;
      use type Database.Jobs.Job_Id;

      Current_Job : constant Database.Jobs.Job_Id
        := Database.Jobs.Get_Current_Job;
      S : Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Jobs.Vector.Is_Empty then
         return "<p>oO  No Jobs  Oo</p>";
      end if;

      Append (S, "<table><tr><th>Ref</th><th>Title</th></tr>");
      for Job of Jobs.Vector loop

         if Job.Id = Current_Job then
            Append (S, "<tr style=""Background-Color:#Dddd2222"">");
         else
            Append (S, "<tr>");
         end if;

--         Append (S, "<td><a href=""/?cmd=set%20job%20" & Job.Ref & """>"
--                   & Job.Ref & "</a></td>");
         Append (S, "<td>" & To_String (Job.Title) & "</td>");
         Append (S, "</tr>");
      end loop;
      Append (S, "</table>");
      return  To_String (S);
   end Jobs_Image;


   function Job_Image (Job : in Database.Jobs.Job_Id)
                      return HTML_String
   is
      use Database.Events;
      use Ada.Strings.Unbounded;

      Info       : constant Database.Jobs.Job_Info
        := Database.Jobs.Get_Job_Info (Job);

      Events     : constant Event_Lists.Vector     := Get_Job_Events (Job);
      Done       : constant Boolean                := Is_Done (Job);
      Done_Image : constant String                 := Boolean'Image (Done);
      A          : Unbounded_String :=
        "<p>Title  (" & Info.Title & ") (Id " & Job'Img & ")</p>" &
        "<p>Parent (Id " & Info.Parent'Img & ")</p>"     &
        "<p>Owner  (" & Info.Owner & "</p>"              &
        "<p>Status DONE: " & Done_Image & "</p>";
   begin
      Append (A, "<table><tr><th>Date Time</th><th>Event</th></tr>");
      for Event of Events loop
         Append (A,
                 "<tr>" &
                   "<td>" & Event.Stamp & "</td>" &
                   "<td>" & Event.Kind  & "</td>" &
                   "</tr>");
      end loop;
      Append (A, "</table>");

      return To_String (A);
   end Job_Image;


end Web_IO;
