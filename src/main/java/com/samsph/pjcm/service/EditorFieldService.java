package com.samsph.pjcm.service;


import com.samsph.pjcm.model.EditorField;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.awt.print.Pageable;
import java.util.List;
import java.util.Optional;


public interface EditorFieldService {

     void addEditorField(EditorField editorField);

     void updateEditorField(EditorField editorField);

     void deleteEditorField(int id);

     Optional<EditorField> findEditorField(int id);

     List<EditorField> findByEditorUid(Integer editorUid);

     Page<EditorField> findEditorFieldsByEditorUid(int editorUid, PageRequest pageRequest);

     Page<EditorField> findEditorFieldsByFieldId(int fieldId, PageRequest pageRequest);
}
