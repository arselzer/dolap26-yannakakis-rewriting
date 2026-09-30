SELECT fx_comment.userid AS g1, fx_vote.userid AS g2, COUNT(*) AS witness_count
FROM badges AS s0,
     posts AS s1,
     users AS s2,
     votes AS s3,
     postlinks AS fx_link,
     posts AS fx_post,
     comments AS fx_comment,
     users AS fx_user,
     votes AS fx_vote,
     users AS fx_voter
WHERE fx_comment.postid = fx_link.relatedpostid
  AND fx_comment.postid = fx_post.id
  AND fx_comment.postid = fx_vote.postid
  AND fx_comment.userid = fx_user.id
  AND fx_link.postid = s1.id
  AND fx_link.postid = s3.postid
  AND fx_vote.userid = fx_voter.id
  AND s0.userid = s1.owneruserid
  AND s0.userid = s2.id
GROUP BY fx_comment.userid, fx_vote.userid;
