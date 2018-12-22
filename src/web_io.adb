--
--
--

with Ada.Strings.Unbounded;

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


   function Jobs_Image (Jobs : in Database.Job_Set)
                       return String
   is
      use Ada.Strings.Unbounded;
      use type Database.Job_Id;

      Current_Job : constant Database.Job_Id := Database.Get_Current_Job;
      S : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (S, "<table><tr><th>Ref</th><th>Title</th></tr>");
      for Job of Jobs.Vector loop

         if Job.Id = Current_Job then
            Append (S, "<tr style=""Background-Color:#Dddd2222"">");
         else
            Append (S, "<tr>");
         end if;

         Append (S, "<td><a href=""/?cmd=set%20job%20" & Job.Ref & """>"
                   & Job.Ref & "</a></td>");
         Append (S, "<td>" & To_String (Job.Title) & "</td>");
         Append (S, "</tr>");
      end loop;
      Append (S, "</table>");
      return  To_String (S);
   end Jobs_Image;


--     function Lists_Image (Lists : in Database.List_Set)
--                          return String
--     is
--        use Ada.Strings.Unbounded;
--        use type Database.List_Id;

--        S : Ada.Strings.Unbounded.Unbounded_String;
--     begin
--        Append (S, "<table><tr><th>Ref</th>"
--                  & "<th>Title</th><th>Description</th></tr>");
--        for List of Lists.Vector loop

--           if List.Id = Lists.Current then
--              Append (S, "<tr style=""Background-Color:#Dddd2222"">");
--           else
--              Append (S, "<tr>");
--           end if;

--           Append (S, "<td>");
--           Append (S, "<a href=""/?cmd=set%20list%20" & List.Ref & """>"
--                     & List.Ref & "</a></td>");
--           Append (S, "<td>");
--           Append (S, List.Name);
--           Append (S, "</td>");
--           Append (S, "<td>");
--           Append (S, List.Desc);
--           Append (S, "</td>");
--           Append (S, "</tr>");
--        end loop;
--        Append (S, "</table>");
--        return To_String (S);
--     end Lists_Image;


   function Job_Image (Job : in Database.Job_Id)
                      return HTML_String
   is
      use Ada.Strings.Unbounded;

      Info   : constant Database.Job_Info  := Database.Get_Job_Info (Job);
      Events : constant Database.Event_Lists.Vector
        := Database.Get_Job_Events (Job);
      A : Unbounded_String;
   begin
      Append (A, "<p>");
      Append (A, To_String (Info.Title) & " (" & Job'Img & ")");
      Append (A, "</p>");
      Append (A, "<p>");
      Append (A, "Parent (" & Info.Parent'Img & ")");
      Append (A, "Owner: " & To_String (Info.Owner));
      Append (A, "</p>");

      Append (A, "<table><tr><th>Date Time</th><th>Event</th></tr>");
      for Event of Events loop
         Append (A, "<tr>");
         Append (A, "<td>" & To_String (Event.Stamp) & "</td>");
         Append (A, "<td>" & To_String (Event.Kind)  & "</td>");
         Append (A, "</tr>");
      end loop;
      Append (A, "</table>");

      return To_String (A);
   end Job_Image;


end Web_IO;
