package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.dao.EditorFieldRepository;
import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.service.EditorFieldService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Service
@Transactional
public class EditorFieldServiceImpl implements EditorFieldService {
    @Autowired
    private EditorFieldRepository editorFieldRepository;

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
    public Page<EditorField> findEditorFieldsByEditorUid(int editorUid, PageRequest pageRequest) {
        EditorField editorField = new EditorField();
        editorField.setEditorUid(editorUid);
        ExampleMatcher exampleMatcher = ExampleMatcher.matching().withMatcher("editorUid",ExampleMatcher.GenericPropertyMatchers.exact());
        Example<EditorField> example = Example.of(editorField,exampleMatcher);
        return editorFieldRepository.findAll(example,pageRequest);
    }

    @Override
    public Page<EditorField> findEditorFieldsByFieldId(int fieldId, PageRequest pageRequest) {
        EditorField editorField = new EditorField();
        editorField.setField(fieldId);
        ExampleMatcher exampleMatcher = ExampleMatcher.matching().withMatcher("field",ExampleMatcher.GenericPropertyMatchers.exact());
        Example<EditorField> example = Example.of(editorField,exampleMatcher);
        return editorFieldRepository.findAll(example,pageRequest);
    }
}
