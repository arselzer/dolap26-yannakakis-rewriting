SELECT fx_comment.userid AS g1, fx_vote.userid AS g2, COUNT(*) AS witness_count
FROM badges AS s0,
     comments AS s1,
     postlinks AS s2,
     posts AS s3,
     users AS s4,
     votes AS s5,
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
  AND fx_comment.userid = s0.userid
  AND fx_comment.userid = s3.owneruserid
  AND fx_comment.userid = s4.id
  AND fx_link.postid = s1.postid
  AND fx_link.postid = s2.relatedpostid
  AND fx_link.postid = s3.id
  AND fx_link.postid = s5.postid
  AND fx_vote.userid = fx_voter.id
GROUP BY fx_comment.userid, fx_vote.userid;
