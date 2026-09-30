SELECT b.userid AS g1, c3.userid AS g2, c.postid AS g3, COUNT(*) AS witness_count
FROM comments AS c,
     posts AS p,
     postlinks AS pl,
     posthistory AS ph,
     votes AS v,
     badges AS b,
     users AS u,
     posts AS p2,
     comments AS c3,
     users AS u3
WHERE b.userid = c.userid
  AND b.userid = u.id
  AND c.postid = p.id
  AND c.postid = ph.postid
  AND c.postid = pl.relatedpostid
  AND c.postid = v.postid
  AND c3.postid = p2.id
  AND c3.postid = pl.postid
  AND c3.userid = u3.id
  AND (c.Score=0 AND p.ViewCount>=0 AND p.AnswerCount<=5 AND p.CommentCount<=12 AND p.FavoriteCount>=0 AND pl.LinkTypeId=1 AND pl.CreationDate>=CAST('2011-02-16 20:04:50' AS TIMESTAMP) AND pl.CreationDate<=CAST('2014-09-01 16:48:04' AS TIMESTAMP) AND v.CreationDate>=CAST('2010-07-19 00:00:00' AS TIMESTAMP) AND v.CreationDate<=CAST('2014-08-31 00:00:00' AS TIMESTAMP) AND b.Date>=CAST('2010-08-06 10:36:45' AS TIMESTAMP) AND b.Date<=CAST('2014-09-12 07:19:35' AS TIMESTAMP))
GROUP BY b.userid, c3.userid, c.postid;
