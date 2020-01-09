package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.vo.PostExportVO;
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
     * 以分页的方式查询投稿人的所有投稿
     *
     * @param uid  投稿人id
     * @param page 分页请求
     * @return 稿件分页
     */
    Page<Post> findByContributorUid(int uid, Pageable page);

    /**
     * 以分页的方式查询投稿人的某段时间内的所有投稿
     *
     * @param uid   投稿人id
     * @param page  分页请求
     * @param start 开始日期
     * @param end   结束日期
     * @return 稿件分页
     */
    Page<Post> findByContributorUidAndSubmitTimeAfterAndSubmitTimeBefore(int uid, Date start, Date end, Pageable page);

    /**
     * 以分页的方式查询某一状态的投稿人的所有投稿
     *
     * @param uid      投稿人id
     * @param page     分页请求
     * @param statuses 投稿状态列表
     * @return 稿件分页
     */
    Page<Post> findByContributorUidAndStatusIn(int uid, List<Integer> statuses, Pageable page);

    /**
     * 以分页的方式查询某一状态的投稿人的某一段时间内的所有投稿
     *
     * @param uid      投稿人id
     * @param page     分页请求
     * @param statuses 投稿状态列表
     * @param start    开始日期
     * @param end      结束日期
     * @return 稿件分页
     */
    Page<Post> findByContributorUidAndStatusInAndSubmitTimeAfterAndSubmitTimeBefore(int uid, List<Integer> statuses, Date start, Date end, Pageable page);

    /**
     * 以分页的方式查询编辑负责的所有投稿
     *
     * @param uid  编辑id
     * @param page 分页请求
     * @return 稿件分页
     */
    Page<Post> findByEditorUid(int uid, Pageable page);

    /**
     * 以分页的方式查询编辑负责的所有投稿
     *
     * @param uid   编辑id
     * @param start 开始日期
     * @param end   结束日期
     * @param page  分页请求
     * @return 稿件分页
     */
    Page<Post> findByEditorUidAndSubmitTimeAfterAndSubmitTimeBefore(int uid, Date start, Date end, Pageable page);

    /**
     * 以分页的方式查询某一状态的编辑负责的所有投稿
     *
     * @param uid      编辑id
     * @param page     分页请求
     * @param statuses 投稿状态列表
     * @return 稿件分页
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
     * @return 稿件分页
     */
    Page<Post> findByEditorUidAndStatusInAndSubmitTimeAfterAndSubmitTimeBefore(int uid, List<Integer> statuses, Date start, Date end, Pageable page);

    /**
     * 以分页的方式查询审稿人未答复的所有投稿
     *
     * @param uid    审稿人id
     * @param accept 接受审稿标识
     * @param page   分页请求
     * @return 稿件分页
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
     * @return 稿件分页
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
     * @return 稿件分页
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
     * @return 稿件分页
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
     * @return 稿件分页
     */
    @Query(value = "SELECT * FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid" +
            " WHERE pr.reviewer_uid = ?1 AND pr.accept = 1 AND pr.flag=0",
            countQuery = "SELECT count(*) FROM post p INNER JOIN post_reviewer pr ON p.id = pr.pid" +
                    " WHERE pr.reviewer_uid = ?1 AND pr.accept = 1 AND pr.flag=0",
            nativeQuery = true)
    Page<Post> findByReviewerUidAndFlagFalse(int uid, Pageable page);

    /**
     * 以分页的形式获取某些状态的所有稿件
     *
     * @param statuses    状态列表
     * @param pageRequest 分页请求
     * @return 稿件分页
     */
    Page<Post> findByStatusIn(List<Integer> statuses, PageRequest pageRequest);

    /**
     * 以分页的形式获取某些状态且在某段时间提交的所有稿件
     *
     * @param statuses    状态列表
     * @param pageRequest 分页请求
     * @param start       开始时间
     * @param end         结束时间
     * @return 稿件分页
     */
    Page<Post> findByStatusInAndSubmitTimeAfterAndSubmitTimeBefore(List<Integer> statuses, Date start, Date end, PageRequest pageRequest);

    /**
     * 以分页的形式获取某些状态、某些单位的所有稿件
     *
     * @param statuses    状态列表
     * @param fAuEmployer 一作的工作单位
     * @param pageRequest 分页请求
     * @return 稿件分页
     */
    Page<Post> findByStatusInAndFAuEmployerLike(List<Integer> statuses, String fAuEmployer, PageRequest pageRequest);

    /**
     * 以分页的形式获取某些状态、某些单位且在某段时间提交的所有稿件
     *
     * @param statuses    状态列表
     * @param fAuEmployer 一作的工作单位
     * @param start       开始日期
     * @param end         结束日期
     * @param pageRequest 分页请求
     * @return 稿件分页
     */
    Page<Post> findByStatusInAndFAuEmployerLikeAndSubmitTimeAfterAndSubmitTimeBefore(List<Integer> statuses, String fAuEmployer, Date start, Date end, PageRequest pageRequest);

    /**
     * 以分页的形式获取某些状态、某些作者的所有稿件
     *
     * @param statuses    状态列表
     * @param fAuName     一作姓名
     * @param pageRequest 分页请求
     * @return 稿件分页
     */
    Page<Post> findByStatusInAndFAuNameLike(List<Integer> statuses, String fAuName, PageRequest pageRequest);

    /**
     * 以分页的形式获取某些状态、某些作者在某段时间提交的所有稿件
     *
     * @param statuses    状态列表
     * @param fAuName     一作姓名
     * @param start       开始时间
     * @param end         结束时间
     * @param pageRequest 分页请求
     * @return 稿件分页
     */
    Page<Post> findByStatusInAndFAuNameLikeAndSubmitTimeAfterAndSubmitTimeBefore(List<Integer> statuses, String fAuName, Date start, Date end, PageRequest pageRequest);

    /**
     * 以分页的形式获取某些状态、某些单位、某些作者在某段时间提交的所有稿件
     *
     * @param statuses    状态列表
     * @param fAuName     一作姓名
     * @param fAuEmployer 一作单位
     * @param pageRequest 分页请求
     * @return 稿件分页
     */
    Page<Post> findByStatusInAndFAuNameLikeAndFAuEmployerLike(List<Integer> statuses, String fAuName, String fAuEmployer, PageRequest pageRequest);

    /**
     * 以分页的形式获取某些状态、某些单位、某些作者在某段时间提交的所有稿件
     *
     * @param statuses    状态列表
     * @param fAuName     一作姓名
     * @param fAuEmployer 一作单位
     * @param start       开始时间
     * @param end         结束时间
     * @param pageRequest 分页请求
     * @return 稿件分页
     */
    Page<Post> findByStatusInAndFAuNameLikeAndFAuEmployerLikeAndSubmitTimeAfterAndSubmitTimeBefore(List<Integer> statuses, String fAuName, String fAuEmployer, Date start, Date end, PageRequest pageRequest);

    /**
     * 以分页的形式获取某些状态、某段时间上传缴费证明的所有稿件
     *
     * @param statuses    状态列表
     * @param start       开始时间
     * @param end         结束时间
     * @param pageRequest 分页请求
     * @return 稿件分页
     */
    Page<Post> findByStatusInAndCertificateUploadTimeAfterAndCertificateUploadTimeBefore(List<Integer> statuses, Date start, Date end, PageRequest pageRequest);

    /**
     * 以分页的形式获取某些状态、某些单位、某段时间上传缴费证明的所有稿件
     *
     * @param statuses    状态列表
     * @param fAuEmployer 一作单位
     * @param start       开始时间
     * @param end         结束时间
     * @param pageRequest 分页请求
     * @return 稿件分页
     */
    Page<Post> findByStatusInAndFAuEmployerLikeAndCertificateUploadTimeAfterAndCertificateUploadTimeBefore(List<Integer> statuses, String fAuEmployer, Date start, Date end, PageRequest pageRequest);

    /**
     * 以分页的形式获取某些状态、某些作者、某段时间上传缴费证明的所有稿件
     *
     * @param statuses    状态列表
     * @param fAuName     一作姓名
     * @param start       开始时间
     * @param end         结束时间
     * @param pageRequest 分页请求
     * @return 稿件分页
     */
    Page<Post> findByStatusInAndFAuNameLikeAndCertificateUploadTimeAfterAndCertificateUploadTimeBefore(List<Integer> statuses, String fAuName, Date start, Date end, PageRequest pageRequest);

    /**
     * 以分页的形式获取某些状态、某些单位、某些作者、某段时间上传缴费证明的所有稿件
     *
     * @param statuses    状态列表
     * @param fAuName     一作姓名
     * @param fAuEmployer 一作单位
     * @param start       开始时间
     * @param end         结束时间
     * @param pageRequest 分页请求
     * @return 稿件分页
     */
    Page<Post> findByStatusInAndFAuNameLikeAndFAuEmployerLikeAndCertificateUploadTimeAfterAndCertificateUploadTimeBefore(List<Integer> statuses, String fAuName, String fAuEmployer, Date start, Date end, PageRequest pageRequest);
}
