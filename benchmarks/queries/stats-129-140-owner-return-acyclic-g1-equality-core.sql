SELECT b.userid AS g1, COUNT(*) AS witness_count
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
GROUP BY b.userid;
