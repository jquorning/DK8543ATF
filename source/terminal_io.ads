--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Types;

package Terminal_IO is

   procedure Put_Jobs;
   --  Put list of jobs from Navigate.List.

   procedure Show_Job (Job : in Types.Job_Id);
   procedure Put_Help;
   procedure Put_Error (Text : in String);
   procedure Put_Banner;
   procedure Put_Path;

end Terminal_IO;
