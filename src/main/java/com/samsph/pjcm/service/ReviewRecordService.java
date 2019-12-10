package com.samsph.pjcm.service;

import com.samsph.pjcm.model.ReviewRecord;
import com.samsph.pjcm.query.ReviewRecordQuery;
import org.springframework.data.domain.Page;

/**
 * 审稿记录表service层接口
 *
 * @author hujiahao
 */
public interface ReviewRecordService {

    /**
     * 保存审稿记录
     *
     * @param reviewRecordQuery 审稿记录请求
     * @param uid               审稿人id
     * @param cnt               处于第几轮审稿
     * @return 审稿记录对象
     */
    ReviewRecord save(ReviewRecordQuery reviewRecordQuery, int uid, int cnt);


    /**
     * 上轮建议修改的审稿人是否都已在此轮提交审稿记录
     *
     * @param pid 稿件id
     * @return 此轮审稿是否可关闭
     */
    boolean canReReviewClose(int pid);

    /**
     * 获得某稿件的所有编辑记录
     *
     * @param pid    稿件id
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 审稿记录页面
     */
    Page<ReviewRecord> getAllByPid(int pid, int number, int size, boolean ascend);

    /**
     * 获得某稿件某审稿人的所有编辑记录
     *
     * @param pid    稿件id
     * @param uid    审稿人id
     * @param number 页号
     * @param size   页面大小
     * @param ascend 是否升序
     * @return 审稿记录页面
     */
    Page<ReviewRecord> getAllByPidAndUid(int pid, int uid, int number, int size, boolean ascend);
}
