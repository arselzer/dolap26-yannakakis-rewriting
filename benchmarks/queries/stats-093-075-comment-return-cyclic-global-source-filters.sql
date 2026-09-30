SELECT COUNT(*) AS witness_count
FROM comments AS c,
     posts AS p,
     posthistory AS ph,
     votes AS v,
     badges AS b,
     users AS u,
     postlinks AS pl2,
     comments AS c2,
     votes AS v2,
     users AS u2
WHERE b.userid = p.owneruserid
  AND b.userid = u.id
  AND c.postid = p.id
  AND c.postid = ph.postid
  AND c.postid = pl2.postid
  AND c.postid = v.postid
  AND c2.postid = pl2.relatedpostid
  AND c2.postid = v2.postid
  AND u2.id = v2.userid
  AND c2.userid = u.id
  AND (c.Score=0 AND c.CreationDate<=CAST('2014-09-09 19:58:29' AS TIMESTAMP) AND p.Score>=-4 AND p.ViewCount>=0 AND p.ViewCount<=5977 AND p.AnswerCount<=4 AND p.CommentCount>=0 AND p.CommentCount<=11 AND p.CreationDate>=CAST('2011-01-25 08:31:41' AS TIMESTAMP) AND u.Reputation<=312 AND u.DownVotes<=0);
