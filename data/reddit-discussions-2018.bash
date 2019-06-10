SUBREDDIT=gameofthrones
mkdir -p "$SUBREDDIT"

for i in {01..10}; do 
  wget http://files.pushshift.io/reddit/comments/RC_2018-$i.xz
  unxz -d RC_2018-$i.xz
  grep "\"subreddit\":\"$SUBREDDIT\"" RC_2018-$i > $SUBREDDIT/RC_2018-$i
  rm RC*
  
  wget http://files.pushshift.io/reddit/submissions/RS_2018-$i.xz
  unxz -d RS_2018-$i.xz
  grep "\"subreddit\":\"$SUBREDDIT\"" RS_2018-$i > $SUBREDDIT/RS_2018-$i
  rm RS*
done

cat $SUBREDDIT/RC_2018-* > $SUBREDDIT/RC_2018-all

for i in {11..12}; do
  wget http://files.pushshift.io/reddit/comments/RC_2018-$i.zst
  zstd -d RC_2018-$i.zst
  grep "\"subreddit\":\"$SUBREDDIT\"" RC_2018-$i > $SUBREDDIT/RC_2018-$i
  rm RC*
  
  wget http://files.pushshift.io/reddit/submissions/RS_2018-$i.zst
  zstd -d RS_2018-$i.zst
  grep "\"subreddit\":\"$SUBREDDIT\"" RS_2018-$i > $SUBREDDIT/RS_2018-$i
  rm RS*
done

cat $SUBREDDIT/RS_2018-* > $SUBREDDIT/RS_2018-all

