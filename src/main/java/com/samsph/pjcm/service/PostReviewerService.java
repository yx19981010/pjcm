package com.samsph.pjcm.service;

import com.samsph.pjcm.model.PostReviewer;
import com.samsph.pjcm.query.PostReviewerQuery;

import java.util.List;

/**
 * @author hujiahao
 */
public interface PostReviewerService {

    /**
     * 增加稿件-审稿人
     *
     * @param postReviewerQuery 稿件-审稿人请求
     * @return PostReviewerSimpleVO
     */
    PostReviewer save(PostReviewerQuery postReviewerQuery);

    /**
     * 查找稿件-审稿人
     *
     * @param pid 稿件id
     * @param uid 审稿人id
     * @return 稿件-审稿人记录
     */
    PostReviewer getPostReviewer(int pid, int uid);

    /**
     * 查找稿件-审稿人
     *
     * @param pid  稿件id
     * @param flag 审稿标识
     * @return 稿件-审稿人列表
     */
    List<PostReviewer> getAllByPidAndFlag(int pid, boolean flag);

    /**
     * 更新稿件-审稿人
     *
     * @param postReviewer 要更新的稿件-审稿人记录
     */
    void updatePostReviewer(PostReviewer postReviewer);

    /**
     * 汇总稿件-审稿人的建议修改情况，如果下轮无人建议修改，则可得到稿件通过
     *
     * @param pid 稿件id
     * @return 汇总结果是否通过
     */
    boolean aggregate(int pid);

    /**
     * 根据id删除稿件-审稿人记录
     *
     * @param id 稿件-审稿人记录id
     */
    void deletePostReviewer(int id);
}
