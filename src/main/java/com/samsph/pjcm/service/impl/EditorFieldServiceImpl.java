package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.dao.EditorFieldRepository;
import com.samsph.pjcm.dao.UserRepository;
import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.model.ReviewerField;
import com.samsph.pjcm.model.User;
import com.samsph.pjcm.model.UserRole;
import com.samsph.pjcm.service.EditorFieldService;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.vo.EditorFieldVoGet;
import com.samsph.pjcm.vo.EditorFieldVoGetField;
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
public class EditorFieldServiceImpl implements EditorFieldService {
    @Autowired
    private EditorFieldRepository editorFieldRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private UserRoleService userRoleService;

    @Override
    public void addEditorField(EditorField editorField) {

        editorFieldRepository.save(editorField);
    }

    @Override
    public void updateEditorField(EditorField editorField) {

        editorFieldRepository.save(editorField);
    }

    @Override
    public void deleteEditorField(int id) {

        editorFieldRepository.deleteById(id);
    }

    @Override
    public Optional<EditorField> findEditorField(int id) {
        return editorFieldRepository.findById(id);
    }

    @Override
    public List<EditorField> findByEditorUid(Integer editorUid) {
        return editorFieldRepository.findByEditorUid(editorUid);
    }

    @Override
    public List<EditorField> findByFieldId(Integer fieldId) {
        List editorFields = new ArrayList(editorFieldRepository.findByField(fieldId));
        Iterator<EditorField> iterator = editorFields.iterator();
        while (iterator.hasNext()) {
            EditorField cur = iterator.next();
            if (userRepository.findById(cur.getEditorUid()).get().getActive() != 1) {
                iterator.remove();
            }
        }
        return editorFields;
    }

    @Override
    public List<EditorFieldVoGet> findAll() {
        List<UserRole> list = userRoleService.findUserRolesByRole(RoleType.EDITOR_ROLE);
        List<EditorFieldVoGet> list1 = new ArrayList<>();
        if(list != null && list.size() > 0) {
            for (UserRole i : list) {
                List<EditorField> list2 = findByEditorUid(i.getUid());
                User user = userRepository.findById(i.getUid()).get();
                List<EditorFieldVoGetField> list3 = DozerUtil.mapList(list2,EditorFieldVoGetField.class);
                //用户未激活或者已注销
                if(user.getActive() != 1){
                    continue;
                }else {
                    EditorFieldVoGet editorFieldVoGet = new EditorFieldVoGet();
                    editorFieldVoGet.setEditorUid(user.getId());
                    editorFieldVoGet.setEmail(user.getEmail());
                    editorFieldVoGet.setUserName(user.getUserName());
                    editorFieldVoGet.setField(list3);
                    list1.add(editorFieldVoGet);
                }
            }
            return list1;
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"系统无对应角色的用户!!!");
        }
    }

    @Override
    public Page<EditorField> findEditorFieldsByFieldId(int fieldId, Pageable pageable) {
        List<EditorField> list = findByFieldId(fieldId);
        if(list != null && list.size() > 0){
            List<Integer> editorFieldList = new ArrayList<>();
            for(EditorField i : list){
                editorFieldList.add(i.getId());
            }
            Specification<EditorField> specification = new Specification<EditorField>() {
                @Override
                public Predicate toPredicate(Root<EditorField> root, CriteriaQuery<?> criteriaQuery, CriteriaBuilder criteriaBuilder) {
                    List<Predicate> list = new ArrayList<>();
                    CriteriaBuilder.In<Object> in = criteriaBuilder.in(root.get("id"));
                    for (Integer id : editorFieldList) {
                        in.value(id);
                    }
                    list.add(in);
                    Predicate[] p = new Predicate[list.size()];
                    return criteriaBuilder.and(list.toArray(p));
                }
            };
            return editorFieldRepository.findAll(specification, pageable);
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"系统领域下无对应的编辑!!!");
        }
    }
}
