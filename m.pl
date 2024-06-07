:- use_module(library(pce)).
:- pce_image_directory('./').

% Define symptoms
symptom(lump_in_breast).
symptom(pain_in_breast).
symptom(skin_irritation_on_breast).
symptom(breast_redness).
symptom(nipple_discharge).
symptom(nipple_retraction).
symptom(swelling_in_breast).
symptom(change_in_breast_shape).
symptom(family_history_of_breast_cancer).
symptom(age_over_50).
symptom(early_menstruation).
symptom(late_menopause).
symptom(hormone_replacement_therapy).
symptom(headache_severity).

% Define diseases and their associated symptoms and risk factors
disease(breast_cancer, [lump_in_breast, pain_in_breast, skin_irritation_on_breast, breast_redness, nipple_discharge, nipple_retraction, swelling_in_breast, change_in_breast_shape, family_history_of_breast_cancer, age_over_50, early_menstruation, late_menopause, hormone_replacement_therapy, headache_severity(high)], high).
disease(benign_breast_conditions, [lump_in_breast, pain_in_breast, skin_irritation_on_breast, nipple_discharge, headache_severity(low)], low).

% Rule to diagnose a disease based on symptoms and assess risk rate
diagnose(Disease, RiskRate) :-
    disease(Disease, Symptoms, RiskRate),
    check_symptoms(Symptoms).

% Check if all symptoms for a disease are present
check_symptoms([]) :-
    format('All symptoms matched.\n').
check_symptoms([Symptom | Rest]) :-
    (has_symptom(Symptom) ->
        (format('Matched symptom: ~w\n', [Symptom]), check_symptoms(Rest));
    (has_symptom(Symptom, Value), member(Symptom(Value), [Symptom|Rest]) ->
        (format('Matched symptom: ~w with value: ~w\n', [Symptom, Value]), check_symptoms(Rest));
        (format('Did not match symptom: ~w\n', [Symptom]), fail))).

% GUI for asking symptoms
ask_symptoms :-
    new(Dialog, dialog('Breast Cancer Diagnosis')),
    send_list(Dialog, append,
              [ new(Lump, menu(lump_in_breast, marked)),
                new(Pain, menu(pain_in_breast, marked)),
                new(Skin, menu(skin_irritation_on_breast, marked)),
                new(Redness, menu(breast_redness, marked)),
                new(Discharge, menu(nipple_discharge, marked)),
                new(Retraction, menu(nipple_retraction, marked)),
                new(Swelling, menu(swelling_in_breast, marked)),
                new(Shape, menu(change_in_breast_shape, marked)),
                new(History, menu(family_history_of_breast_cancer, marked)),
                new(Age, menu(age_over_50, marked)),
                new(Early, menu(early_menstruation, marked)),
                new(Late, menu(late_menopause, marked)),
                new(Hormone, menu(hormone_replacement_therapy, marked)),
                new(Headache, menu(headache_severity, cycle)),
                button(submit, message(@prolog, process_input,
                                       Lump?selection, Pain?selection, Skin?selection, Redness?selection,
                                       Discharge?selection, Retraction?selection, Swelling?selection, Shape?selection,
                                       History?selection, Age?selection, Early?selection, Late?selection,
                                       Hormone?selection, Headache?selection))
              ]),
    send_list(Lump, append, [menu_item(yes), menu_item(no)]),
    send_list(Pain, append, [menu_item(yes), menu_item(no)]),
    send_list(Skin, append, [menu_item(yes), menu_item(no)]),
    send_list(Redness, append, [menu_item(yes), menu_item(no)]),
    send_list(Discharge, append, [menu_item(yes), menu_item(no)]),
    send_list(Retraction, append, [menu_item(yes), menu_item(no)]),
    send_list(Swelling, append, [menu_item(yes), menu_item(no)]),
    send_list(Shape, append, [menu_item(yes), menu_item(no)]),
    send_list(History, append, [menu_item(yes), menu_item(no)]),
    send_list(Age, append, [menu_item(yes), menu_item(no)]),
    send_list(Early, append, [menu_item(yes), menu_item(no)]),
    send_list(Late, append, [menu_item(yes), menu_item(no)]),
    send_list(Hormone, append, [menu_item(yes), menu_item(no)]),
    send_list(Headache, append, [low, medium, high]),
    send(Dialog, open).

% Process the input from the GUI
process_input(Lump, Pain, Skin, Redness, Discharge, Retraction, Swelling, Shape, History, Age, Early, Late, Hormone, Headache) :-
    retractall(has_symptom(_)),
    retractall(has_symptom(_, _)),
    retractall(no_symptom(_)),
    map_symptom(lump_in_breast, Lump),
    map_symptom(pain_in_breast, Pain),
    map_symptom(skin_irritation_on_breast, Skin),
    map_symptom(breast_redness, Redness),
    map_symptom(nipple_discharge, Discharge),
    map_symptom(nipple_retraction, Retraction),
    map_symptom(swelling_in_breast, Swelling),
    map_symptom(change_in_breast_shape, Shape),
    map_symptom(family_history_of_breast_cancer, History),
    map_symptom(age_over_50, Age),
    map_symptom(early_menstruation, Early),
    map_symptom(late_menopause, Late),
    map_symptom(hormone_replacement_therapy, Hormone),
    map_symptom(headache_severity, Headache),
    diagnose_and_print.

diagnose_and_print :-
    (diagnose(Disease, RiskRate) ->
        (format('You may have ~w.\n', [Disease]),
         format('Risk rate: ~w\n', [RiskRate]));
        write('Sorry, no diagnosis can be made with the given symptoms.\n')).

% Helper predicate to assert symptoms
map_symptom(Symptom, yes) :-
    assert(has_symptom(Symptom)),
    format('Added symptom: ~w\n', [Symptom]).
map_symptom(Symptom, no) :-
    assert(no_symptom(Symptom)),
    format('Added no_symptom: ~w\n', [Symptom]).
map_symptom(Symptom, Value) :-
    Value \= yes,
    Value \= no,
    assert(has_symptom(Symptom, Value)),
    format('Added symptom: ~w with value: ~w\n', [Symptom, Value]).

% Start the GUI for diagnosis
:- ask_symptoms.

