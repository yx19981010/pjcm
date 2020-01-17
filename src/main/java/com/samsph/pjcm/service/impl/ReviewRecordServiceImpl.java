package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.config.constant.MyBoolean;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.dao.PostRepository;
import com.samsph.pjcm.dao.ReviewRecordRepository;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.model.ReviewRecord;
import com.samsph.pjcm.query.ReviewRecordQuery;
import com.samsph.pjcm.service.ReviewRecordService;
import com.samsph.pjcm.service.UserService;
import com.samsph.pjcm.vo.ReviewRecordVO;
import com.samsph.pjcm.vo.ReviewerFieldVoGetField;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
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

    @Resource
    private UserService userService;

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

        if(reviewRecordQuery.getReject()==null || !reviewRecordQuery.getReject()){
            reviewRecord.setReject(0);
        }else{
            reviewRecord.setReject(1);
        }
        reviewRecord.setRejectComment(reviewRecordQuery.getRejectComment());

        if(reviewRecordQuery.getToForward() == null || !reviewRecordQuery.getToForward()){
            reviewRecord.setToForward(0);
        }else {
            reviewRecord.setToForward(1);
        }
        reviewRecord.setForwardComment(reviewRecordQuery.getForwardComment());

        if(reviewRecordQuery.getToRevise() == null ||!reviewRecordQuery.getToRevise()){
            reviewRecord.setToRevise(0);
        }else {
            reviewRecord.setToRevise(1);
        }

        reviewRecord.setReviseComment(reviewRecordQuery.getReviseComment());

        reviewRecord.setText(reviewRecordQuery.getText());
        reviewRecord.setScientific(reviewRecordQuery.getScientific());
        reviewRecord.setPracticality(reviewRecordQuery.getPracticality());
        reviewRecord.setPolitical(reviewRecordQuery.getPolitical());
        reviewRecord.setEvaluation(reviewRecordQuery.getEvaluation());
        reviewRecord.setAdviceToNewsroom(reviewRecordQuery.getAdviceToNewsroom());
        reviewRecord.setAcademic(reviewRecordQuery.getAcademic());
        reviewRecord.setPublish(reviewRecordQuery.getPublish());

        return reviewRecordRepository.save(reviewRecord);
    }

    @Override
    public boolean canReReviewClose(int pid) {
        Optional<Post> postOptional = postRepository.findById(pid);
        if (!postOptional.isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "稿件不存在");
        }

        int cnt = postOptional.get().getCount();

        int thisRoundTotal = reviewRecordRepository.findByPidAndCount(pid, cnt).size();
        int lastRoundRevise = reviewRecordRepository.findByPidAndCountAndToRevise(
                pid, cnt - 1, MyBoolean.TRUE.getCode()).size();

        return thisRoundTotal == lastRoundRevise;
    }

    @Override
    public List<ReviewRecord> findByPublishAndPidAndCount(int publish,int pid,int count) {
        return reviewRecordRepository.findByPublishAndPidAndCount(publish,pid,count);
    }

    @Override
    public List<ReviewRecordVO> getAllByPid(int pid) {
        List<ReviewRecord> list = reviewRecordRepository.findByPid(pid);
        List<ReviewRecordVO> list1 = DozerUtil.mapList(list,ReviewRecordVO.class);
        for(ReviewRecordVO reviewRecordVo : list1){
            reviewRecordVo.setReview_name(userService.findUserByUid(reviewRecordVo.getUid()).get().getUserName());
        }
        return list1;
    }

    @Override
    public List<ReviewRecord> getAllByPidAndUid(int pid, int uid) {
        return reviewRecordRepository.findByPidAndUid(pid, uid);
    }
}
