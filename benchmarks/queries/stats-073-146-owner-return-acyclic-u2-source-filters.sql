SELECT c.postid AS g1, c3.userid AS g2, COUNT(*) AS witness_count
FROM comments AS c,
     posts AS p,
     postlinks AS pl,
     posthistory AS ph,
     votes AS v,
     users AS u,
     posts AS p2,
     comments AS c3,
     users AS u3
WHERE c.postid = p.id
  AND c.postid = ph.postid
  AND c.postid = pl.postid
  AND c.postid = v.postid
  AND c3.postid = p2.id
  AND c3.postid = pl.relatedpostid
  AND c3.userid = u3.id
  AND p.owneruserid = u.id
  AND (c.CreationDate>=CAST('2010-07-26 19:37:03' AS TIMESTAMP) AND p.Score>=-2 AND p.CommentCount<=18 AND p.CreationDate>=CAST('2010-07-21 13:50:08' AS TIMESTAMP) AND p.CreationDate<=CAST('2014-09-11 00:53:10' AS TIMESTAMP) AND pl.CreationDate<=CAST('2014-08-05 18:27:51' AS TIMESTAMP) AND ph.CreationDate>=CAST('2010-11-27 03:38:45' AS TIMESTAMP) AND u.DownVotes>=0 AND u.UpVotes>=0)
GROUP BY c.postid, c3.userid;
