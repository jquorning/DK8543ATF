--
--
--

with Ada.Strings.Unbounded;

with Database.Jobs;
with Database.Events;
with Commands;
with Navigate;
with Symbols;

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


   function Jobs_Image return String
   is
      use Navigate;
      use Ada.Strings.Unbounded;
      use type Types.Job_Id;

      S : Ada.Strings.Unbounded.Unbounded_String;
   begin
      if List.Set.Is_Empty then
         return "<p>oO  No Jobs  Oo</p>";
      end if;

      Append (S, "<table><tr><th>DN</th><th>Ref</th><th>Title</th></tr>");
      for Index in List.Set.First_Index .. List.Set.Last_Index loop

         --  Highlight
         if List.Set (Index).Id = List.Current then
            Append (S, "<tr style=""Background-Color:#Dddd2222"">");
         else
            Append (S, "<tr>");
         end if;

         --  DONE
         if Database.Events.Is_Done (List.Set (Index).Id) then
            Append (S, "<td>" & Symbols.HTML (Symbols.Black_Star) & "</td>");
         else
            Append (S, "<td>" & Symbols.HTML (Symbols.White_Star) & "</td>");
         end if;

         --  Reference
         Append (S, "<td><a href=""/?cmd=set%20job%20"
                   & List.Refs (Index).Ref & """>"
                   & List.Refs (Index).Ref & "</a></td>");

         --  Title
         if List.Refs (Index).Level = 0 then
            Append (S, "<td>" & To_String (List.Set (Index).Title) & "</td>");
         else
            Append (S, "<td>" & "___" & To_String (List.Set (Index).Title)
                      & "</td>");
         end if;

         Append (S, "</tr>");
      end loop;
      Append (S, "</table>");
      return  To_String (S);
   end Jobs_Image;


   function Job_Image (Job : in Types.Job_Id)
                      return HTML_String
   is
      use Database.Events;
      use Ada.Strings.Unbounded;

      Info       : constant Types.Job_Info
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
