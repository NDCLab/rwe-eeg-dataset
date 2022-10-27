#!/bin/bash
IFS=$'\n'

# init proj specific variables
dataset="/home/data/NDClab/datasets/rwe-eeg-dataset/"
tasks="rwe-eeg_s1_r1_e1,read-aloud-val-eeg_s1_r1_e1,flanker-basic-v5_s1_r1_e1"
filetypes="audio,digi,bv,psychopy,redcap"
logfile="${dataset}data-monitoring/data-monitoring-log.md"

# load in functions & variables
source /home/data/NDClab/tools/lab-devOps/scripts/monitor/tools.sh

usage() { echo "Usage: sh hallMonitor.sh [-m/-r] [string list of replacement or mapping]" 1>&2; exit 1; }

error_detected=false
for dir in `ls $raw`
do
    # If psychopy or pavlovia dataset
    if [[ ${pavpsy[*]} =~ $dir ]]; then
        echo "Accessing $raw/$dir"
        cd $raw/$dir

        # store dir names in array
        sub_names=(*/)
        for i in "${!sub_names[@]}"; do
            subject=${sub_names[$i]}

            # if no pavlovia dataset exists in checked, create
            if [ ! -e "$check/$dir" ]; then
                mkdir $check/$dir
            fi

            # check if name is properly named and copy if correct
            sub_check=$(verify_copy_sub $dir $subject)
            res=$?
            if [ $res != 0 ]; then
                echo -e "$sub_check"
                echo -e "\t ${RED}Error detected in $subject. View above.${NC} \n" 
                error_detected=true
                continue 
            fi
            echo -e "\t Checking files of $raw/$dir/$subject"
            cd $raw/$dir/$subject

            # check if files contain all tasks, appropriatley named, 
            # and contain correct ID's
            files_log=$(verify_copy_pavpsy_files $dir $subject $tasks)
            res=$?
            if [[ $res != 0 || "$files_log" =~ "Error:" ]]; then
                echo -e "$files_log"
                echo -e "\t ${RED}Error detected in $subject. View above${NC} \n"
                error_detected=true
                continue 
            else 
                echo -e "$files_log"
                echo -e "\t ${GREEN}Success. All data passes checks in $subject.${NC}"
            fi
        done
        echo -e "\n"
        # update tracker for each id
        output=$( python ${dataset}data-monitoring/update-tracker.py "${check}/${dir}" $dir $dataset $tasks)
        if [[ "$output" =~ "Error" ]]; then
            echo -e "\t $output \n \t ${RED}Error detected in checked $dir data.${NC}"
            error_detected=true
        fi
        echo $output
        echo -e "\n"             
    fi
    # If zoom, audio, or video dataset
    if [[ ${audivid[*]} =~ $dir ]]; then
        echo "Accessing $raw/$dir"
        # update tracker for each id
        output=$( python ${dataset}data-monitoring/update-tracker.py "${check}/${dir}" $dir $dataset)
        if [[ "$output" =~ "Error" ]]; then
            echo -e "\t $output \n \t ${RED}Error detected in checked $dir data.${NC}"
            error_detected=true
            continue
        fi
        echo $output
        echo -e "\n"
    fi
    # If redcap dataset
    if [ "$dir" == "$redcap" ]; then
        echo "Accessing $raw/$dir"
        cd $raw/$dir

        # store file names in array and get most recent file, check if stem is correct
        file_name=$( get_new_redcap )

        if [[ "$file_name" =~ "Error:" ]]; then
            echo -e "$file_name"
            echo -e "\t ${RED}Error detected in $dir. View above${NC}"
            error_detected=true
            continue
        fi
        echo -e "\t Newest file found: $file_name"
        
        # move only if data does not already exist in checked
        if [ -f "$check/$dir/$file_name" ]; then
            echo -e "\t $dir/$file_name already exists in checked, skipping copy \n"
            continue
        fi

        echo -e "\t ${GREEN}Data passes criteria${NC}"

        # if redcap does not exist in checked, create it
        if [ ! -e "$check/$dir" ]; then
            mkdir $check/$dir
        fi
        echo -e "\t copying $file_name to $check/$dir"
        cp $raw/$dir/$file_name $check/$dir

        # rename columns in checked using replace or map
        while getopts ":rm" opt; do
            case ${opt} in
                r)
                    python ${dataset}data-monitoring/rename-cols.py $check/$dir/$file_name "replace" $2 ;;
                m)
                    python ${dataset}data-monitoring/rename-cols.py $check/$dir/$file_name "map" $2 ;;
                :)
            esac 
        done

        # update tracker
        output=$( python ${dataset}data-monitoring/update-tracker.py $file_name $dir $dataset)
        if [[ "$output" =~ "Error" ]]; then
            echo -e "\t $output \n \t ${RED}Error detected in $file_name.${NC}"
            error_detected=true
            continue
        fi
        echo $output
        echo -e "\n"
    fi
    if [[ ${eegtype[*]} =~ $dir ]]; then
        # if no bidsish dataset exists in checked, create
        if [ ! -e "${check}/${eeg}" ]; then
            mkdir $check/$eeg
        fi

        echo "Accessing $raw/$dir"
        cd $raw/$dir
        sub_names=(*/)
        for i in "${!sub_names[@]}"; do
            subject=${sub_names[$i]}
            sub_check=$(verify_copy_sub $eeg/$subject $dir "bids")
            res=$?
            if [ $res != 0 ]; then
                echo -e "$sub_check"
                echo -e "\t ${RED}Error detected in $subject. View above.${NC} \n" 
                error_detected=true
                continue 
            fi

            echo -e "\t Checking files of $raw/$dir/$subject"
            cd $raw/$dir/$subject
            files_log=$(verify_copy_bids_files $dir $subject $tasks $filetypes)
            res=$?
            if [[ $res != 0 || "$files_log" =~ "Error:" ]]; then
                echo -e "$files_log"
                echo -e "\t ${RED}Error detected in $subject. View above${NC} \n"
                error_detected=true
                continue 
            else 
                echo -e "$files_log"
                echo -e "\t ${GREEN}Success. All data passes checks in $subject.${NC}"
            fi

        done
        output=$( python ${dataset}data-monitoring/update-tracker.py "${check}/${eeg}" $dir $dataset)
        if [[ "$output" =~ "Error" ]]; then
            echo -e "\t $output \n \t ${RED}Error detected in checked $dir data.${NC}"
            error_detected=true
            continue
        fi
        echo $output
        echo -e "\n"
    fi
done        
cd ${dataset}data-monitoring/
if [ $error_detected = true ]; then
    update_log "error" $logfile
else
    update_log "success" $logfile
fi

