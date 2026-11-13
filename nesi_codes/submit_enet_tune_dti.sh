# submit an array job per dataset to process datasets and batches in parallel

# there are 21 site left in the cleaned dataset
N_BATCHES=21


for DATASET in DTI_FA_AT_FS_.RDS  DTI_LD_AT_FS_.RDS  DTI_MD_AT_FS_.RDS DTI_TD_AT_FS_.RDS  DTI_VOL_AT_FS_.RDS
do      
  DATANAME=$(basename $DATASET .RDS)
  JOBID=$(sbatch -J enet_tune.${DATANAME} --array=1-${N_BATCHES} enet_tune.sh ${DATASET} )
  JOBID=$(echo $JOBID | awk '{print $4}')
  echo "Job submitted: dataset=$DATASET arraysize=$N_BATCHES job=$JOBID"

done