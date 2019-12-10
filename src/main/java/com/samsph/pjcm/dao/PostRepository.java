package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.Post;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;


/**
 * 投稿表数据操作接口
 *
 * @author hujiahao
 */
public interface PostRepository extends JpaRepository<Post, Integer> {

    /**
     * 获取某一期刊包含的投稿列表
     *
     * @param jid  期刊id
     * @param page 分页请求
     * @return List<Post>
     */
    @Query(value = "SELECT * FROM post p WHERE p.jid=?1",
            countQuery = "SELECT count(*) FROM post p WHERE p.jid=?1",
            nativeQuery = true)
    Page<Post> findAllByJid(int jid, Pageable page);


    /**
     * 以分页的方式查询投稿人的所有投稿
     *
     * @param uid  投稿人id
     * @param page 分页请求
     * @return Page<Post>
     */
    @Query(value = "SELECT * FROM post p WHERE p.contributor_uid=?1",
            countQuery = "SELECT count(*) FROM post p WHERE p.contributor_uid=?1",
            nativeQuery = true)
    Page<Post> findAllByCtr(int uid, Pageable page);

    /**
     * 以分页的方式查询某一状态的投稿人的所有投稿
     *
     * @param uid    投稿人id
     * @param page   分页请求
     * @param status 投稿状态
     * @return Page<Post>
     */
    @Query(value = "SELECT * FROM post p WHERE p.contributor_uid=?1 AND p.status=?2",
            countQuery = "SELECT count(*) FROM post p WHERE p.contributor_uid=?1 AND p.status=?2",
            nativeQuery = true)
    Page<Post> findAllByCtrAndStatus(int uid, int status, Pageable page);

    /**
     * 以分页的方式查询编辑负责的所有投稿
     *
     * @param uid  编辑id
     * @param page 分页请求
     * @return Page<Post>
     */
    @Query(value = "SELECT * FROM post p WHERE p.editor_uid=?1",
            countQuery = "SELECT count(*) FROM post p WHERE p.editor_uid=?1",
            nativeQuery = true)
    Page<Post> findAllByEd(int uid, Pageable page);

    /**
     * 以分页的方式查询某一状态的编辑负责的所有投稿
     *
     * @param uid    编辑id
     * @param page   分页请求
     * @param status 投稿状态
     * @return Page<Post>
     */
    @Query(value = "SELECT * FROM post p WHERE p.editor_uid=?1 AND p.status=?2",
            countQuery = "SELECT count(*) FROM post p WHERE p.editor_uid=?1 AND p.status=?2",
            nativeQuery = true)
    Page<Post> findAllByEdAndStatus(int uid, int status, Pageable page);

    /**
     * 以分页的方式查询审稿人未答复的所有投稿
     *
     * @param uid    审稿人id
     * @param accept 接受审稿标识
     * @param page   分页请求
     * @return Page<Post>
     */
    @Query(value = "SELECT * FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid WHERE pr.reviewer_uid = ?1 AND pr.accepted = ?2",
            countQuery = "SELECT count(*) FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid WHERE pr.reviewer_uid = ?1 AND pr.accepted = ?2",
            nativeQuery = true)
    Page<Post> findAllByRevUidAndAccept(int uid, int accept, Pageable page);

    /**
     * 以分页的方式查询审稿人已接受且处于审稿中的所有投稿
     *
     * @param uid  审稿人id
     * @param page 分页请求
     * @return Page<Post>
     */
    @Query(value = "SELECT * FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid WHERE pr.reviewer_uid = ?1 AND pr.accepted = 1 AND (p.status=4 OR p.status=9)",
            countQuery = "SELECT count(*) FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid WHERE pr.reviewer_uid = ?1 AND pr.accepted = 1 AND (p.status=4 OR p.status=9)",
            nativeQuery = true)
    Page<Post> findAllByRev(int uid, Pageable page);

    /**
     * 以分页的方式查询审稿人已接受且处于审稿中的所有投稿
     *
     * @param uid  审稿人id
     * @param page 分页请求
     * @return Page<Post>
     */
    @Query(value = "SELECT * FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid" +
            " WHERE pr.reviewer_uid = ?1 AND pr.accepted = 1 AND pr.flag=?2 AND (p.status=4 OR p.status=9)",
            countQuery = "SELECT count(*) FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid" +
                    " WHERE pr.reviewer_uid = ?1 AND pr.accepted = 1 AND pr.flag=?2 AND (p.status=4 OR p.status=9)",
            nativeQuery = true)
    Page<Post> findAllRequiredToReview(int uid, int flag, Pageable page);
}
