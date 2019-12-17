package com.samsph.pjcm.service.impl;
import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.dao.ReviewerFieldRepository;
import com.samsph.pjcm.dao.UserRepository;
import com.samsph.pjcm.model.ReviewerField;
import com.samsph.pjcm.model.User;
import com.samsph.pjcm.model.UserRole;
import com.samsph.pjcm.service.ReviewerFieldService;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.vo.ReviewerFieldVoGet;
import com.samsph.pjcm.vo.ReviewerFieldVoGetField;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

@Service
@Transactional
public class ReviewerFieldServiceImpl implements ReviewerFieldService {
    @Autowired
    private ReviewerFieldRepository reviewerFieldRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private UserRoleService userRoleService;

    @Override
    public void addReviewerField(ReviewerField reviewerField) {

        reviewerFieldRepository.save(reviewerField);
    }

    @Override
    public void updateReviewerField(ReviewerField reviewerField) {

        reviewerFieldRepository.save(reviewerField);
    }

    @Override
    public void deleteReviewerField(int id) {

        reviewerFieldRepository.deleteById(id);
    }

    @Override
    public Optional<ReviewerField> findReviewerField(int id) {

        return reviewerFieldRepository.findById(id);
    }

    @Override
    public List<ReviewerField> findByReviewerUid(Integer reviewerUid) {
        return reviewerFieldRepository.findByReviewerUid(reviewerUid);
    }

    @Override
    public List<ReviewerField> findByFieldId(Integer fieldId) {
        List reviewerFields = new ArrayList(reviewerFieldRepository.findByField(fieldId));
        Iterator<ReviewerField> iterator = reviewerFields.iterator();
        while (iterator.hasNext()) {
            ReviewerField cur = iterator.next();
            if (userRepository.findById(cur.getReviewerUid()).get().getActive() != 1) {
                iterator.remove();
            }
        }
        return reviewerFields;
    }

    @Override
    public List<ReviewerFieldVoGet> findAll() {
        List<UserRole> list = userRoleService.findUserRolesByRole(RoleType.REVIEWER_ROLE);
        List<ReviewerFieldVoGet> list1 = new ArrayList<>();
        if(list != null && list.size() > 0) {
            for (UserRole i : list) {
                List<ReviewerField> list2 = findByReviewerUid(i.getUid());
                User user = userRepository.findById(i.getUid()).get();
                List<ReviewerFieldVoGetField> list3 = DozerUtil.mapList(list2,ReviewerFieldVoGetField.class);
                //用户未激活或者已注销
                if(user.getActive() != 1){
                    continue;
                }else {
                    ReviewerFieldVoGet reviewerFieldVoGet = new ReviewerFieldVoGet();
                    reviewerFieldVoGet.setReviewerUid(user.getId());
                    reviewerFieldVoGet.setEmail(user.getEmail());
                    reviewerFieldVoGet.setUserName(user.getUserName());
                    reviewerFieldVoGet.setField(list3);
                    list1.add(reviewerFieldVoGet);
                }
            }
            return list1;
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"系统无对应角色的用户!!!");
        }
    }

    @Override
    public Page<ReviewerField> findReviewerFieldsByFieldId(int fieldId, Pageable pageable) {
        List<ReviewerField> list = findByFieldId(fieldId);
        if(list != null && list.size() > 0){
            List<Integer> reviewerFieldList = new ArrayList<>();
            for(ReviewerField i : list){
                reviewerFieldList.add(i.getId());
            }
            Specification<ReviewerField> specification = new Specification<ReviewerField>() {
                @Override
                public Predicate toPredicate(Root<ReviewerField> root, CriteriaQuery<?> criteriaQuery, CriteriaBuilder criteriaBuilder) {
                    List<Predicate> list = new ArrayList<>();
                    CriteriaBuilder.In<Object> in = criteriaBuilder.in(root.get("id"));
                    for (Integer id : reviewerFieldList) {
                        in.value(id);
                    }
                    list.add(in);
                    Predicate[] p = new Predicate[list.size()];
                    return criteriaBuilder.and(list.toArray(p));
                }
            };
            return reviewerFieldRepository.findAll(specification, pageable);
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"系统领域下无对应的审稿人!!!");
        }
    }
}
