package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.config.constant.MyBoolean;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.dao.PostRepository;
import com.samsph.pjcm.dao.ReviewRecordRepository;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.model.ReviewRecord;
import com.samsph.pjcm.query.ReviewRecordQuery;
import com.samsph.pjcm.service.ReviewRecordService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Date;
import java.util.Optional;

/**
 * @author hujahao
 */
@Service
public class ReviewRecordServiceImpl implements ReviewRecordService {

    @Resource
    private PostRepository postRepository;

    @Resource
    private ReviewRecordRepository reviewRecordRepository;

    @Override
    public ReviewRecord save(ReviewRecordQuery reviewRecordQuery, int uid, int cnt) {
        int pid = reviewRecordQuery.getPid();

        Optional<ReviewRecord> reviewRecordOptional = reviewRecordRepository.findByPidAndUidAndCount(pid, uid, cnt);

        // 若存在，则抛出异常
        if (reviewRecordOptional.isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "审稿记录已存在");
        }

        ReviewRecord reviewRecord = new ReviewRecord();
        reviewRecord.setPid(pid);
        reviewRecord.setUid(uid);
        reviewRecord.setCount(cnt);
        reviewRecord.setCreateTime(new Date());

        reviewRecord.setReject(reviewRecordQuery.getReject() ? 1 : 0);
        reviewRecord.setRejectComment(reviewRecordQuery.getRejectComment());

        reviewRecord.setToForward(reviewRecordQuery.getToForward() ? 1 : 0);
        reviewRecord.setForwardComment(reviewRecordQuery.getForwardComment());

        reviewRecord.setToRevise(reviewRecordQuery.getToRevise() ? 1 : 0);
        reviewRecord.setReviseComment(reviewRecordQuery.getReviseComment());

        reviewRecord.setText(reviewRecordQuery.getText());
        reviewRecord.setScientific(reviewRecordQuery.getScientific());
        reviewRecord.setPracticality(reviewRecordQuery.getPracticality());
        reviewRecord.setPolitical(reviewRecordQuery.getPolitical());
        reviewRecord.setEvaluation(reviewRecordQuery.getEvaluation());
        reviewRecord.setAdviceToNewsroom(reviewRecordQuery.getAdviceToNewsroom());
        reviewRecord.setAcademic(reviewRecordQuery.getAcademic());

        return reviewRecordRepository.save(reviewRecord);
    }

    @Override
    public boolean canReReviewClose(int pid) {
        Optional<Post> postOptional = postRepository.findById(pid);
        if (postOptional.isEmpty()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "稿件不存在");
        }

        int cnt = postOptional.get().getCount();

        int thisRoundTotal = reviewRecordRepository.findByPidAndCount(pid, cnt).size();
        int lastRoundRevise = reviewRecordRepository.findByPidAndCountAndToRevise(
                pid, cnt - 1, MyBoolean.TRUE.getCode()).size();

        return thisRoundTotal == lastRoundRevise;
    }

    @Override
    public Page<ReviewRecord> getAllByPid(int pid, int number, int size, boolean ascend) {
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size,direction,"createTime");
        return reviewRecordRepository.findByPid(pid, pageRequest);
    }

    @Override
    public Page<ReviewRecord> getAllByPidAndUid(int pid, int uid, int number, int size, boolean ascend) {
        Sort.Direction direction = ascend ? Sort.Direction.ASC : Sort.Direction.DESC;
        PageRequest pageRequest = PageRequest.of(number - 1, size, direction,"createTime");
        return reviewRecordRepository.findByPidAndUid(pid, uid, pageRequest);
    }
}
