--
--  TODO
--  Status spec
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
