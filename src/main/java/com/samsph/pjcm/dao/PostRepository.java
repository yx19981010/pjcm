package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.Post;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Date;
import java.util.List;


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
    Page<Post> findByJid(int jid, Pageable page);


    /**
     * 以分页的方式查询投稿人的所有投稿
     *
     * @param uid  投稿人id
     * @param page 分页请求
     * @return Page<Post>
     */
    Page<Post> findByContributorUid(int uid, Pageable page);

    /**
     * 以分页的方式查询投稿人的某段时间内的所有投稿
     *
     * @param uid   投稿人id
     * @param page  分页请求
     * @param start 开始日期
     * @param end   结束日期
     * @return Page<Post>
     */
    Page<Post> findByContributorUidAndSubmitTimeAfterAndSubmitTimeBefore(int uid, Date start, Date end, Pageable page);

    /**
     * 以分页的方式查询某一状态的投稿人的所有投稿
     *
     * @param uid    投稿人id
     * @param page   分页请求
     * @param statuses 投稿状态列表
     * @return Page<Post>
     */
    Page<Post> findByContributorUidAndStatusIn(int uid, List<Integer> statuses, Pageable page);

    /**
     * 以分页的方式查询某一状态的投稿人的某一段时间内的所有投稿
     *
     * @param uid    投稿人id
     * @param page   分页请求
     * @param statuses 投稿状态列表
     * @param start  开始日期
     * @param end    结束日期
     * @return Page<Post>
     */
    Page<Post> findByContributorUidAndStatusInAndSubmitTimeAfterAndSubmitTimeBefore(int uid, List<Integer> statuses, Date start, Date end, Pageable page);

    /**
     * 以分页的方式查询编辑负责的所有投稿
     *
     * @param uid  编辑id
     * @param page 分页请求
     * @return Page<Post>
     */
    Page<Post> findByEditorUid(int uid, Pageable page);

    /**
     * 以分页的方式查询编辑负责的所有投稿
     *
     * @param uid   编辑id
     * @param start 开始日期
     * @param end   结束日期
     * @param page  分页请求
     * @return Page<Post>
     */
    Page<Post> findByEditorUidAndSubmitTimeAfterAndSubmitTimeBefore(int uid, Date start, Date end, Pageable page);

    /**
     * 以分页的方式查询某一状态的编辑负责的所有投稿
     *
     * @param uid      编辑id
     * @param page     分页请求
     * @param statuses 投稿状态列表
     * @return Page<Post>
     */
    Page<Post> findByEditorUidAndStatusIn(int uid, List<Integer> statuses, Pageable page);

    /**
     * 以分页的方式查询某一状态的编辑负责的所有投稿
     *
     * @param uid      编辑id
     * @param start    开始日期
     * @param end      结束日期
     * @param statuses 投稿状态列表
     * @param page     分页请求
     * @return Page<Post>
     */
    Page<Post> findByEditorUidAndStatusInAndSubmitTimeAfterAndSubmitTimeBefore(int uid, List<Integer> statuses, Date start, Date end, Pageable page);

    /**
     * 以分页的方式查询审稿人未答复的所有投稿
     *
     * @param uid    审稿人id
     * @param accept 接受审稿标识
     * @param page   分页请求
     * @return Page<Post>
     */
    @Query(value = "SELECT * FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid WHERE pr.reviewer_uid = ?1 AND pr.accept = ?2",
            countQuery = "SELECT count(*) FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid WHERE pr.reviewer_uid = ?1 AND pr.accept = ?2",
            nativeQuery = true)
    Page<Post> findByReviewerUidAndAccept(int uid, int accept, Pageable page);

    /**
     * 以分页的方式查询审稿人莫某段时间内未答复的所有投稿
     *
     * @param uid    审稿人id
     * @param start  开始日期
     * @param end    结束日期
     * @param accept 接受审稿标识
     * @param page   分页请求
     * @return Page<Post>
     */
    @Query(value = "SELECT * FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid " +
            "WHERE pr.reviewer_uid = ?1 AND pr.accept = ?2 AND p.submit_time BETWEEN ?3 AND ?4",
            countQuery = "SELECT count(*) FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid " +
                    "WHERE pr.reviewer_uid = ?1 AND pr.accept = ?2 AND p.submit_time > ?3 AND p.submit_time < ?4",
            nativeQuery = true)
    Page<Post> findByReviewerUidAndAcceptAndSubmitTimeAfterAndSubmitTimeBefore(int uid, int accept, Date start, Date end, Pageable page);

    /**
     * 以分页的方式查询审稿人已接受且处于审稿中的所有投稿
     *
     * @param uid   审稿人id
     * @param flag  是否需要审稿标识
     * @param start 开始日期
     * @param end   结束日期
     * @param page  分页请求
     * @return Page<Post>
     */
    @Query(value = "SELECT * FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid" +
            " WHERE pr.reviewer_uid = ?1 AND pr.accept = 1 AND pr.flag=?2 AND p.submit_time BETWEEN ?3 AND ?4",
            countQuery = "SELECT count(*) FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid" +
                    " WHERE pr.reviewer_uid = ?1 AND pr.accept = 1 AND pr.flag=?2 AND p.submit_time > ?3 AND p.submit_time < ?4",
            nativeQuery = true)
    Page<Post> findByReviewerUidAndFlagAndSubmitTimeAfterAndSubmitTimeBefore(int uid, int flag, Date start, Date end, Pageable page);

    /**
     * 以分页的方式查询审稿人已接受且处于审稿中的所有投稿
     *
     * @param uid  审稿人id
     * @param page 分页请求
     * @return Page<Post>
     */
    @Query(value = "SELECT * FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid" +
            " WHERE pr.reviewer_uid = ?1 AND pr.accept = 1 AND pr.flag=1 AND (p.status=4 OR p.status=9)",
            countQuery = "SELECT count(*) FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid" +
                    " WHERE pr.reviewer_uid = ?1 AND pr.accept = 1 AND pr.flag=1 AND (p.status=4 OR p.status=9)",
            nativeQuery = true)
    Page<Post> findByReviewerUidAndFlagTrue(int uid, Pageable page);

    /**
     * 以分页的方式查询审稿人已接受且处于审稿中的所有投稿
     *
     * @param uid  审稿人id
     * @param page 分页请求
     * @return Page<Post>
     */
    @Query(value = "SELECT * FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid" +
            " WHERE pr.reviewer_uid = ?1 AND pr.accept = 1 AND pr.flag=0",
            countQuery = "SELECT count(*) FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid" +
                    " WHERE pr.reviewer_uid = ?1 AND pr.accept = 1 AND pr.flag=0",
            nativeQuery = true)
    Page<Post> findByReviewerUidAndFlagFalse(int uid, Pageable page);
}
