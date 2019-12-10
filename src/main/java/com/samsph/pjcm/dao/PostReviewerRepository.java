package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.PostReviewer;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

/**
 * @author hujiahao
 */
public interface PostReviewerRepository extends JpaRepository<PostReviewer, Integer> {

    /**
     * 通过稿件id、审稿人id查询记录
     *
     * @param pid       稿件id
     * @param reviewUid 审稿人id
     * @return Optional<PostReviewer>
     */
    Optional<PostReviewer> findByPidAndReviewerUid(int pid, int reviewUid);

    /**
     * 获得某稿件的建议修改的审稿人
     *
     * @param pid  稿件id
     * @param flag 下轮审稿标识
     * @return 稿件-审稿人记录列表
     */
    List<PostReviewer> findByPidAndFlag(int pid, boolean flag);
}
