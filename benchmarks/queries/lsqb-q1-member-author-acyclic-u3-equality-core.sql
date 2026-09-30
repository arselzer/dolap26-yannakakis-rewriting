SELECT city.ispartof_countryid AS g1, tag.hastype_tagclassid AS g2, comment.commentid AS g3, COUNT(*) AS witness_count
FROM country AS country,
     city AS city,
     person AS person,
     forum_hasmember_person AS forum_hasmember_person,
     forum AS forum,
     post AS post,
     comment AS comment,
     comment_hastag_tag AS comment_hastag_tag,
     tag AS tag,
     tagclass AS tagclass
WHERE city.cityid = person.islocatedin_cityid
  AND city.ispartof_countryid = country.countryid
  AND comment.commentid = comment_hastag_tag.commentid
  AND comment.replyof_postid = post.postid
  AND comment_hastag_tag.tagid = tag.tagid
  AND forum.forumid = forum_hasmember_person.forumid
  AND forum.forumid = post.forum_containerofid
  AND forum_hasmember_person.personid = person.personid
  AND tag.hastype_tagclassid = tagclass.tagclassid
GROUP BY city.ispartof_countryid, tag.hastype_tagclassid, comment.commentid;
