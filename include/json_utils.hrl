-define(json_to_record(RecAtom, Rec),
        rfc4627:to_record(Rec, #RecAtom{}, record_info(fields, RecAtom))).
