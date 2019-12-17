package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.ReviewRecord;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

/**
 * @author hujiahao
 */
public interface ReviewRecordRepository extends JpaRepository<ReviewRecord, Integer> {
    /**
     * 根据稿件id、审稿人id和审稿轮数获得审稿记录
     *
     * @param pid   稿件id
     * @param count 审稿轮数
     * @param uid   审稿人id
     * @return 审稿记录
     */
    Optional<ReviewRecord> findByPidAndUidAndCount(int pid, int uid, int count);

    /**
     * 根据稿件id、审稿轮数、是否建议修改获得审稿记录列表
     *
     * @param pid      稿件id
     * @param count    审稿轮数
     * @param toRevise 是否建议修改
     * @return 审稿记录列表
     */
    List<ReviewRecord> findByPidAndCountAndToRevise(int pid, int count, int toRevise);

    /**
     * 根据稿件id、审稿轮数获得审稿记录列表
     *
     * @param pid   稿件id
     * @param count 审稿轮数
     * @return 审稿记录列表
     */
    List<ReviewRecord> findByPidAndCount(int pid, int count);

    /**
     * 根据publish的值获得审稿记录列表
     *
     * @param publish
     * @return
     */
    List<ReviewRecord> findByPublishAndPidAndCount(int publish,int pid,int count);

    /**
     * 获得某稿件的审稿记录
     *
     * @param pid 稿件id
     * @return 稿件记录分页
     */
    @Query(value = "SELECT * FROM review_record p WHERE p.pid=?1",
            nativeQuery = true)
    List<ReviewRecord> findByPid(int pid);

    /**
     * 以分页形式获得某稿件某审稿人的审稿记录
     *
     * @param pid 稿件id
     * @param uid 审稿人id
     * @return 稿件记录分页
     */
    @Query(value = "SELECT * FROM review_record p WHERE p.pid=?1 AND p.uid=?2",
            nativeQuery = true)
    List<ReviewRecord> findByPidAndUid(int pid, int uid);
}
