-define(json_to_record(RecAtom, Rec),
        json_utils:to_record(Rec, #RecAtom{}, record_info(fields, RecAtom))).
