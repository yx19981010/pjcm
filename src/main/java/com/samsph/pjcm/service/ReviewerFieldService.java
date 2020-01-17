package com.samsph.pjcm.service;
import com.samsph.pjcm.model.ReviewerField;
import com.samsph.pjcm.vo.ReviewerFieldVoGet;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Optional;


public interface ReviewerFieldService {

     /**
      * 添加审稿人领域
      * @param reviewerField 审稿人领域实体
      */
     void addReviewerField(ReviewerField reviewerField);

     /**
      *更新审稿人领域
      * @param reviewerField 审稿人领域实体
      */
     void updateReviewerField(ReviewerField reviewerField);

     /**
      *根据审稿人领域id删除审稿人领域
      * @param id 审稿人领域id
      */
     void deleteReviewerField(int id);

     /**
      *根据审稿人领域id得到审稿人领域
      * @param id 审稿人领域id
      * @return 审稿人领域
      */
     Optional<ReviewerField> findReviewerField(int id);

     /**
      *根据审稿人id得到审稿人领域列表
      * @param reviewerUid 审稿人id
      * @return 审稿人领域列表
      */
     List<ReviewerField> findByReviewerUid(Integer reviewerUid);

     /**
      *根据领域id得到审稿人领域列表
      * @param fieldId 领域id
      * @return 审稿人领域列表
      */
     List<ReviewerField> findByFieldId(Integer fieldId);

     /**
      *得到所有审稿人领域列表
      * @return 审稿人领域列表
      */
     List<ReviewerFieldVoGet> findAll();

     /**
      *得到选择审稿人的所有审稿人领域列表
      * @return 审稿人领域列表
      */
     List<ReviewerFieldVoGet> findSelect(int id);

     /**
      *根据领域id得到分页后的审稿人领域列表
      * @param fieldId 领域id
      * @param pageable 分页参数
      * @return 分页后的审稿人领域列表
      */
     Page<ReviewerField> findReviewerFieldsByFieldId(int fieldId, Pageable pageable);

}
