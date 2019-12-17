package com.samsph.pjcm.service;


import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.vo.EditorFieldVoGet;
import com.samsph.pjcm.vo.ReviewerFieldVoGet;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Optional;


public interface EditorFieldService {

     void addEditorField(EditorField editorField);

     void updateEditorField(EditorField editorField);

     void deleteEditorField(int id);

     Optional<EditorField> findEditorField(int id);

     List<EditorField> findByEditorUid(Integer editorUid);

     List<EditorField> findByFieldId(Integer fieldId);

     List<EditorFieldVoGet> findAll();

     Page<EditorField> findEditorFieldsByFieldId(int fieldId, Pageable pageable);
}
