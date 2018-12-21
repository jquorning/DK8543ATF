--
--
--

with Ada.Strings.Unbounded;

package body Web_IO is


   function Jobs_Image (Jobs : in Database.Job_Set)
                       return String
   is
      use Ada.Strings.Unbounded;
      use type Database.Job_Id;

      S : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (S, "<table><tr><th>Ref</th><th>Title</th></tr>");
      for Job of Jobs.Vector loop

         if Job.Id = Jobs.Current then
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


   function Lists_Image (Lists : in Database.List_Set)
                        return String
   is
      use Ada.Strings.Unbounded;
      use type Database.List_Id;

      S : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (S, "<table><tr><th>Ref</th>"
                & "<th>Title</th><th>Description</th></tr>");
      for List of Lists.Vector loop

         if List.Id = Lists.Current then
            Append (S, "<tr style=""Background-Color:#Dddd2222"">");
         else
            Append (S, "<tr>");
         end if;

         Append (S, "<td>");
         Append (S, "<a href=""/?cmd=set%20list%20" & List.Ref & """>"
                   & List.Ref & "</a></td>");
         Append (S, "<td>");
         Append (S, List.Name);
         Append (S, "</td>");
         Append (S, "<td>");
         Append (S, List.Desc);
         Append (S, "</td>");
         Append (S, "</tr>");
      end loop;
      Append (S, "</table>");
      return To_String (S);
   end Lists_Image;


end Web_IO;
