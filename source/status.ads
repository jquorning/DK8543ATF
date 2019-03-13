--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package Status is

   type State_Type  is
     (Fresh,      --  New job on list
      Executing,  --  Job is being done
      Done,       --  Job has completed
      Omitted,    --  Job is abandoned
      Deleted     --  Job is removed
     );

   type Status_Type is
      record
         State     : State_Type;
         Partly    : Boolean;
         Uncertain : Boolean;
      end record;

end Status;
